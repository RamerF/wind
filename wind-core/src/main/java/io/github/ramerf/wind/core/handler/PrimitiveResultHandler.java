package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.sql.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 基本类型结果转换,注意该类使用valueOf转换值,如果没有该方法将会报错.
 *
 * @param <E> the type parameter
 * @author ramer
 * @since 2019 /12/27
 */
@Slf4j
public class PrimitiveResultHandler<E> extends AbstractResultHandler<E> {
  private final Class<E> clazz;

  public PrimitiveResultHandler(@Nonnull final Class<E> clazz) {
    this.clazz = clazz;
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public E handle(ResultSet rs) throws SQLException {
    ResultSetMetaData rsmd = rs.getMetaData();
    int columnCount = rsmd.getColumnCount();
    final Object value = rs.getObject(1);
    if (value == null) {
      return null;
    }
    final Class<?> valueClass = value.getClass();
    if (valueClass.equals(clazz)) {
      return (E) value;
    }
    if (valueClass.isArray()) {
      return (E) value;
    }
    if (InterEnum.class.isAssignableFrom(clazz)) {
      final Class<? extends InterEnum> cls = (Class<? extends InterEnum>) clazz;
      return (E) InterEnum.ofNullable(value, cls);
    }
    // 使用构造器
    try {
      return clazz.getConstructor(valueClass).newInstance(value);
    } catch (Exception e) {
      log.warn("handle:[msg:{},class:{}]", e.getMessage(), e.getClass());
      // 使用valueOf
      try {
        return (E) clazz.getMethod("valueOf", valueClass).invoke(null, value);
      } catch (Exception ex) {
        log.warn("handle:[msg:{},class:{}]", ex.getMessage(), ex.getClass());
      }
    }
    return (E) value;
  }
}
