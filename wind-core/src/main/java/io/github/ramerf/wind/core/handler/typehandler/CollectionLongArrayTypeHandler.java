package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:List<Long> <=> jdbc:Long[]}.
 *
 * @author ramer
 * @since 2020/3/4
 */
public class CollectionLongArrayTypeHandler implements ITypeHandler<Collection<Long>, Long[]> {
  @Override
  public Object convertToJdbc(
      Collection<Long> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal.toArray());
    } catch (SQLException e) {
      throw new CommonException(e);
    }
  }

  @Override
  @SuppressWarnings("DuplicatedCode")
  public Collection<Long> convertFromJdbc(
      final Long[] jdbcVal, final Object defaultValue, final Field field) {
    if (defaultValue == null) {
      return List.class.isAssignableFrom(field.getType())
          ? (jdbcVal == null ? new ArrayList<>() : Arrays.asList(jdbcVal))
          : (jdbcVal == null ? new HashSet<>() : new HashSet<>(Arrays.asList(jdbcVal)));
    }
    @SuppressWarnings("unchecked")
    final Collection<Long> initial = (Collection<Long>) defaultValue;
    initial.clear();
    initial.addAll(Arrays.asList(jdbcVal));
    return initial;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "bigint");
  }
}
