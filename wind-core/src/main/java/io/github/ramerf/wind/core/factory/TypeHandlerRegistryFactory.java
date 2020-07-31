package io.github.ramerf.wind.core.factory;

import io.github.ramerf.wind.core.handler.*;
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
 * @since 2020 /3/28
 */
@Slf4j
@SuppressWarnings({"rawtypes", "unused"})
public class TypeHandlerRegistryFactory {
  private Set<ITypeHandler> typeHandlers =
      new TreeSet<>(((o1, o2) -> Objects.equals(o1.getClass(), o2.getClass()) ? 0 : 1));

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
          .forEach(this::addTypeHandlers);
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

  /**
   * 获取Jdbc值转换为Java值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * <p>valueType {@link ValueType}
   *
   * @param valueType the value type
   * @return the type handler
   * @see ITypeHandler
   */
  @SuppressWarnings("DuplicatedCode")
  public ITypeHandler getToJavaTypeHandler(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (Objects.isNull(value)) {
      return null;
    }
    final ITypeHandler typeHandler = getHandlerFromAnnotation(valueType);
    if (typeHandler != null) {
      return typeHandler;
    }
    final Type genericParameterType = valueType.getGenericParameterType();
    return getTypeHandlers().stream()
        .filter(
            handler -> {
              final Type javaClass = handler.getJavaClass();
              final Type jdbcClass = handler.getJdbcClass();
              try {
                return (Objects.equals(javaClass, genericParameterType)
                        || Class.forName(javaClass.getTypeName())
                            .isAssignableFrom(
                                Class.forName(
                                    Objects.requireNonNull(genericParameterType).getTypeName())))
                    && Objects.equals(value.getClass(), jdbcClass);
              } catch (ClassNotFoundException ignored) {
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }

  /**
   * 获取Java值转换为Jdbc值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * @param valueType {@link ValueType}
   * @return the type handler
   * @see ITypeHandler
   */
  @SuppressWarnings("DuplicatedCode")
  public ITypeHandler getToJdbcTypeHandler(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (Objects.isNull(value)) {
      return null;
    }
    final ITypeHandler typeHandler = getHandlerFromAnnotation(valueType);
    if (typeHandler != null) {
      return typeHandler;
    }
    final Type genericParameterType = valueType.getGenericParameterType();
    return getTypeHandlers().stream()
        .filter(
            handler -> {
              final Type javaClass = handler.getJavaClass();
              try {
                return Objects.equals(javaClass, genericParameterType)
                    || Class.forName(javaClass.getTypeName())
                        .isAssignableFrom(Class.forName(genericParameterType.getTypeName()));
              } catch (ClassNotFoundException ignored) {
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }

  private ITypeHandler getHandlerFromAnnotation(final ValueType valueType) {
    final Field field = valueType.getField();
    final TypeHandler typeHandler = field.getAnnotation(TypeHandler.class);
    if (Objects.nonNull(typeHandler)) {
      try {
        return typeHandler.value().newInstance();
      } catch (InstantiationException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    }
    return null;
  }
}
