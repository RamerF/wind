package io.github.ramerf.wind.core.handler.typehandler;

import com.alibaba.fastjson.JSON;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * 集合对象转JSON数组字符串. {@literal java:List<?> <=> jdbc:json arr string}.
 *
 * @author ramer
 * @since 2020.12.27
 */
@IgnoreScan
public class ObjectCollectionToJsonTypeHandler implements ITypeHandler<Collection<?>, String> {

  @Override
  public Object convertToJdbc(
      final Collection<?> objects, final Field field, @Nonnull final PreparedStatement ps) {
    return JSON.toJSONString(objects);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Collection<?> convertFromJdbc(
      final String jdbcVal, final Object defaultValue, final Field field) {
    final Class<?> fieldType = field.getType();
    if (Collection.class.isAssignableFrom(fieldType)) {
      final Type genericType = field.getGenericType();
      final Type typeArgument = ((ParameterizedType) genericType).getActualTypeArguments()[0];
      if (defaultValue != null) {
        @SuppressWarnings("rawtypes")
        final Collection initial = (Collection) defaultValue;
        initial.clear();
        initial.addAll(JSON.parseArray(jdbcVal, (Class<?>) typeArgument));
        return initial;
      }
      if (typeArgument instanceof Class) {
        return List.class.isAssignableFrom(fieldType)
            ? JSON.parseArray(jdbcVal, (Class<?>) typeArgument)
            : new HashSet<>(JSON.parseArray(jdbcVal, (Class<?>) typeArgument));
      }
      throw new IllegalStateException("不支持的参数类型");
    }
    return List.class.isAssignableFrom(fieldType)
        ? JSON.parseArray(jdbcVal, (Class<?>) fieldType)
        : new HashSet<>(JSON.parseArray(jdbcVal, (Class<?>) fieldType));
  }
}
