package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.IOException;
import java.lang.reflect.*;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 注册类型转换器.
 *
 * @author ramer
 * @since 2020/3/28
 */
@Slf4j
@SuppressWarnings({"rawtypes"})
public class TypeHandlerRegistryFactory {
  private static final List<ITypeHandler> typeHandlers;
  /** 缓存字段类型处理器. */
  private static final Map<Field, ITypeHandler> toJavaTypeHandlers;

  private static final Map<Class<? extends ITypeHandler>, ITypeHandler> typeHandlerMap;

  static {
    typeHandlers = new LinkedList<>();
    toJavaTypeHandlers = Collections.synchronizedMap(new WeakHashMap<>());
    typeHandlerMap = Collections.synchronizedMap(new WeakHashMap<>());
    // 注册默认类型转换器
    try {
      BeanUtils.scanClasses(
              TypeHandlerRegistryFactory.class.getPackage().getName(), ITypeHandler.class)
          .stream()
          .filter(clazz -> !clazz.equals(ITypeHandler.class))
          // 跳过类型处理器
          .filter(clazz -> !clazz.isAnnotationPresent(Skip.class))
          .map(BeanUtils::initial)
          .forEach(
              typeHandler -> {
                typeHandlerMap.put(typeHandler.getClass(), typeHandler);
                log.debug("registerDefaultTypeHandlers:[{}]", typeHandler.getClass().getName());
                addTypeHandlers(typeHandler);
              });
    } catch (IOException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
  }

  /** 添加自定义类型转换器,后添加的优先级更高. */
  public static void addTypeHandlers(@Nonnull ITypeHandler... typeHandlers) {
    addTypeHandlers(Arrays.asList(typeHandlers));
  }

  /** 添加自定义类型转换器,后添加的优先级更高. */
  public static void addTypeHandlers(@Nonnull List<ITypeHandler> typeHandlers) {
    for (int i = typeHandlers.size() - 1; i >= 0; i--) {
      ITypeHandler typeHandler = typeHandlers.get(i);
      TypeHandlerRegistryFactory.typeHandlers.add(0, typeHandler);
    }
  }

  public static List<ITypeHandler> getTypeHandlers() {
    return typeHandlers;
  }

  public static ITypeHandler getTypeHandler(Class<? extends ITypeHandler> clazz) {
    ITypeHandler typeHandler = typeHandlerMap.get(clazz);
    if (typeHandler == null) {
      try {
        typeHandler = clazz.newInstance();
        typeHandlerMap.put(clazz, typeHandler);
      } catch (InstantiationException | IllegalAccessException e) {
        throw new WindException("Need default constructor for typeHandler" + clazz, e);
      }
    }
    return typeHandler;
  }

  /** 获取Jdbc值转换为Java值类型转换器,用于将数据库值转换为Java类型. */
  @SuppressWarnings({"DuplicatedCode", "unchecked"})
  public static ITypeHandler getToJavaTypeHandler(final ValueType valueType) {
    if (toJavaTypeHandlers.containsKey(valueType.getField())) {
      return toJavaTypeHandlers.get(valueType.getField());
    }
    ITypeHandler typeHandler = getHandlerFromAnnotation(valueType);
    if (typeHandler != null) {
      toJavaTypeHandlers.put(valueType.getField(), typeHandler);
      return typeHandler;
    }
    // 默认构造器不处理null值
    final Object value = valueType.getOriginVal();
    if (value == null) {
      return null;
    }
    final Type genericParameterType = valueType.getGenericParameterType();
    typeHandler =
        getTypeHandlers().stream()
            .filter(
                handler -> {
                  final Type javaClass = handler.getJavaClass();
                  final Type jdbcClass = handler.getJdbcClass();
                  boolean eqJavaClass = false;
                  if (Objects.equals(javaClass, genericParameterType)) {
                    eqJavaClass = true;
                  } else {
                    if (javaClass instanceof Class && genericParameterType instanceof Class) {
                      Class javaClazz = (Class) javaClass;
                      Class paramClazz = (Class) genericParameterType;
                      eqJavaClass = javaClazz.isAssignableFrom(paramClazz);
                    } else {
                      try {
                        if (javaClass instanceof ParameterizedType
                            && genericParameterType instanceof ParameterizedType) {
                          final ParameterizedType javaClassType = (ParameterizedType) javaClass;
                          final ParameterizedType parameterType =
                              (ParameterizedType) genericParameterType;
                          eqJavaClass =
                              Class.forName(javaClassType.getRawType().getTypeName())
                                      .isAssignableFrom(
                                          Class.forName(parameterType.getRawType().getTypeName()))
                                  && javaClassType.getActualTypeArguments()[0].equals(
                                      parameterType.getActualTypeArguments()[0]);
                        } else {
                          eqJavaClass =
                              Class.forName(javaClass.getTypeName())
                                  .isAssignableFrom(
                                      Class.forName(genericParameterType.getTypeName()));
                        }
                      } catch (ClassNotFoundException ignored) {
                      }
                    }
                  }
                  boolean eqJdbcClass = false;
                  if (Objects.equals(value.getClass(), jdbcClass)) {
                    eqJdbcClass = true;
                  } else {
                    try {
                      eqJdbcClass =
                          Class.forName(jdbcClass.getTypeName()).isAssignableFrom(value.getClass());
                    } catch (ClassNotFoundException ignored) {
                    }
                  }
                  return eqJavaClass && eqJdbcClass;
                })
            .findFirst()
            .orElse(null);
    toJavaTypeHandlers.put(valueType.getField(), typeHandler);
    return typeHandler;
  }

  /**
   * 获取Java值转换为Jdbc值类型转换器,用于将Java值转换为数据库值.<br>
   *
   * <p>转换为jdbc值时,只有字段注解了类型转换器或者{@link InterEnum}的子类会用到,其余返回原值
   */
  public static ITypeHandler getToJdbcTypeHandler(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (value == null) {
      return null;
    }
    if (valueType.getField() == null) {
      return null;
    }
    final ITypeHandler typeHandler = getHandlerFromAnnotation(valueType);
    if (typeHandler != null) {
      return typeHandler;
    }
    final Class<?> type = valueType.getField().getType();
    if (InterEnum.class.isAssignableFrom(type)) {
      return typeHandlerMap.get(EnumTypeHandler.class);
    }
    if (Date.class.isAssignableFrom(type)) {
      return typeHandlerMap.get(DateTypeHandler.class);
    }
    if (Collection.class.isAssignableFrom(type)) {
      ParameterizedType parameterizedType =
          (ParameterizedType) valueType.getField().getGenericType();
      final Type[] arguments = parameterizedType.getActualTypeArguments();
      if (arguments.length > 0) {
        final Class<?> argument = (Class<?>) arguments[0];
        if (Integer.class.isAssignableFrom(argument)) {
          return typeHandlerMap.get(CollectionIntegerArrayTypeHandler.class);
        }
        if (Long.class.isAssignableFrom(argument)) {
          return typeHandlerMap.get(CollectionLongArrayTypeHandler.class);
        }
        if (String.class.isAssignableFrom(argument)) {
          return typeHandlerMap.get(CollectionStringArrayTypeHandler.class);
        }
      }
    }
    return null;
  }

  private static ITypeHandler getHandlerFromAnnotation(final ValueType valueType) {
    final Field field = valueType.getField();
    final TypeHandler typeHandler = field.getAnnotation(TypeHandler.class);
    return typeHandler == null ? null : getTypeHandler(typeHandler.value());
  }
}
