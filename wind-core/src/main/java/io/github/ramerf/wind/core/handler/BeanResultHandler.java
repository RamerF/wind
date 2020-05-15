package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.helper.TypeConverterHelper;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Method;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/27
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {

  public BeanResultHandler(@Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns);
  }

  @Override
  @SuppressWarnings({"rawtypes", "unchecked"})
  public E handle(Map<String, Object> map) {
    // map = {alia:value}
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final E obj = BeanUtils.initial(clazz);

    for (Method method : super.methods) {
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
      final Object finalValue =
          TypeConverterHelper.toJavaValue(
              value, method.getGenericParameterTypes()[0], parameterType);
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
    return obj;
  }

  @Override
  public List<E> handle(List<Map<String, Object>> maps) {
    if (CollectionUtils.isEmpty(maps)) {
      return Collections.emptyList();
    }
    return maps.stream().map(this::handle).collect(Collectors.toList());
  }
}
