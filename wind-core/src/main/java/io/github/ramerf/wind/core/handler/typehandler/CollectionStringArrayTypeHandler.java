package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:List<String> <=> jdbc:String[]}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class CollectionStringArrayTypeHandler
    implements ITypeHandler<Collection<String>, String[]> {
  @Override
  public Object convertToJdbc(
      Collection<String> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      return ps.getConnection().createArrayOf(getArrayType(field), javaVal.toArray(new String[0]));
    } catch (SQLException e) {
      throw new WindException(e);
    }
  }

  @Override
  @SuppressWarnings("DuplicatedCode")
  public Collection<String> convertFromJdbc(
      final String[] jdbcVal, final Object defaultValue, final Field field) {
    if (defaultValue == null) {
      return List.class.isAssignableFrom(field.getType())
          ? (jdbcVal == null ? new ArrayList<>() : Arrays.asList(jdbcVal))
          : (jdbcVal == null ? new HashSet<>() : new HashSet<>(Arrays.asList(jdbcVal)));
    }
    @SuppressWarnings("unchecked")
    final Collection<String> initial = (Collection<String>) defaultValue;
    initial.clear();
    initial.addAll(Arrays.asList(jdbcVal));
    return initial;
  }
}
