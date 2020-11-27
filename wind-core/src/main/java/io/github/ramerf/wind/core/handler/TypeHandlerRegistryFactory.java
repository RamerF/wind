package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.handler.typehandler.*;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 注册类型转换器.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/28
 */
@Slf4j
@SuppressWarnings({"rawtypes"})
public class TypeHandlerRegistryFactory {
  private Set<ITypeHandler> typeHandlers = new HashSet<>();
  /** 缓存字段类型处理器. */
  private static final Map<Field, ITypeHandler> toJavaTypeHandlers =
      Collections.synchronizedMap(new WeakHashMap<>());

  private static final Map<Class<? extends ITypeHandler>, ITypeHandler> typeHandlerMap =
      Collections.synchronizedMap(new WeakHashMap<>());

  /** Instantiates a new Type handler registry factory. */
  public TypeHandlerRegistryFactory() {}

  /** 注册默认的类型转换器,在添加自定义转换器之后添加这些,保持自定义的转换器优先级更高. */
  public void registerDefaultTypeHandlers() {
    try {
      BeanUtils.scanClasses("io.github.ramerf.wind.core.handler.typehandler", ITypeHandler.class)
          .stream()
          .peek(clazz -> log.info("registerDefaultTypeHandlers:[{}]", clazz.getName()))
          .filter(clazz -> !clazz.equals(ITypeHandler.class))
          // 过滤掉特殊的类型处理器
          .filter(clazz -> !clazz.equals(LongTimestampTypeHandler.class))
          .map(BeanUtils::initial)
          .forEach(
              typeHandler -> {
                typeHandlerMap.put(typeHandler.getClass(), typeHandler);
                addTypeHandlers(typeHandler);
              });
    } catch (IOException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
  }

  /**
   * 添加类型转换器.
   *
   * @param typeHandlers the {@link ITypeHandler}
   * @see ITypeHandler
   */
  public void addTypeHandlers(@Nonnull ITypeHandler... typeHandlers) {
    this.typeHandlers.addAll(Arrays.asList(typeHandlers));
  }

  /**
   * Add type handler.
   *
   * @param typeHandlers the list of handler
   */
  public void addTypeHandlers(@Nonnull Set<ITypeHandler> typeHandlers) {
    CollectionUtils.doIfNonEmpty(typeHandlers, o -> this.typeHandlers.addAll(typeHandlers));
  }

  /**
   * 设置类型转换器,将会覆盖默认的类型转换器.
   *
   * @param typeHandlers the type handler
   */
  public void setTypeHandlers(Set<ITypeHandler> typeHandlers) {
    this.typeHandlers = typeHandlers;
  }

  /**
   * Gets type handler.
   *
   * @return the type handler
   */
  public Set<ITypeHandler> getTypeHandlers() {
    return typeHandlers;
  }

  public ITypeHandler getTypeHandler(Class<? extends ITypeHandler> clazz) {
    ITypeHandler typeHandler = typeHandlerMap.get(clazz);
    if (typeHandler == null) {
      try {
        typeHandler = clazz.newInstance();
        typeHandlerMap.put(clazz, typeHandler);
      } catch (InstantiationException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    }
    return typeHandler;
  }

  /**
   * 获取Jdbc值转换为Java值类型转换器,用于将数据库值转换为Java类型.<br>
   *
   * <p>valueType {@link ValueType}
   *
   * @param valueType the value type
   * @return the type handler
   * @see ITypeHandler
   */
  @SuppressWarnings({"DuplicatedCode", "unchecked"})
  public ITypeHandler getToJavaTypeHandler(final ValueType valueType) {
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
                        eqJavaClass =
                            Class.forName(javaClass.getTypeName())
                                .isAssignableFrom(
                                    Class.forName(genericParameterType.getTypeName()));
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
   *
   * @param valueType {@link ValueType}
   * @return the type handler
   * @see ITypeHandler
   */
  public ITypeHandler getToJdbcTypeHandler(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (Objects.isNull(value)) {
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
    return null;
  }

  private ITypeHandler getHandlerFromAnnotation(final ValueType valueType) {
    final Field field = valueType.getField();
    final TypeHandler typeHandler = field.getAnnotation(TypeHandler.class);
    return typeHandler == null ? null : getTypeHandler(typeHandler.value());
  }
}
