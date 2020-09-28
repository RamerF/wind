package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/27
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {
  /** 返回代理对象,支持自动查询关联对象. */
  /** 方法对应的字段. */
  private static final Map<Method, WeakReference<Field>> METHODS_FIELD_MAP =
      new ConcurrentHashMap<>();

  public BeanResultHandler(@Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns);
  }

  @Override
  public E handle(Map<String, Object> map) {
    // map = {alia:value}
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final E obj = BeanUtils.initial(clazz);

    for (Method method : super.methods) {
      // 跳过? !BeanUtils.isPrimitiveType(method.getReturnType())
      if (AbstractEntityPoJo.class.isAssignableFrom(method.getParameterTypes()[0])) {
        continue;
      }

      final String fieldName = BeanUtils.methodToProperty(method.getName());
      final String columnAlia = fieldAliaMap.get(fieldName);
      Object value =
          Optional.ofNullable(map.get(columnAlia))
              .orElseGet(() -> map.get(StringUtils.camelToUnderline(fieldName)));
      if (Objects.isNull(value)) {
        continue;
      }
      // 如果是数据库数组类型,获取对应的java数组
      if (value instanceof Array) {
        try {
          value = ((Array) value).getArray();
        } catch (SQLException e) {
          log.warn("handle:fail to get array[{}]", e.getMessage());
          log.error(e.getMessage(), e);
        }
      }

      // 判断数据类型,调用指定的转换器,获取到对应的Java值,如果没有就直接赋值.
      final Class<?> parameterType = method.getParameterTypes()[0];
      final Field field = getField(method, fieldName);
      final Object finalValue =
          TypeHandlerHelper.toJavaValue(
              ValueType.of(value, method.getGenericParameterTypes()[0], field), parameterType);
      BeanUtils.invoke(obj, method, finalValue)
          .ifPresent(
              exception ->
                  log.warn(
                      "handle:跳过类型不匹配的字段[fieldName:{},paramType:{},valueType:{}]",
                      fieldName,
                      parameterType.getSimpleName(),
                      Optional.ofNullable(finalValue)
                          .map(Object::getClass)
                          .map(Class::getSimpleName)
                          .orElse(null)));
    }
    if (obj instanceof AbstractEntityPoJo) {
      // 关联对象设置代理
      EntityHelper.getEntityInfo(((AbstractEntityPoJo) obj).getClass())
          .getMappingInfos()
          .forEach(
              mappingInfo ->
                  BeanUtils.setValue(
                      obj,
                      mappingInfo.getField(),
                      mappingInfo
                          .getMappingType()
                          .fetchMapping(
                              (AbstractEntityPoJo) obj,
                              mappingInfo,
                              map.get(mappingInfo.getColumn())),
                      null));
    }
    return obj;
  }

  private Field getField(final Method method, final String fieldName) {
    return Optional.ofNullable(METHODS_FIELD_MAP.get(method))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final Field field = BeanUtils.getDeclaredField(clazz, fieldName);
              METHODS_FIELD_MAP.put(method, new WeakReference<>(field));
              return field;
            });
  }
}
