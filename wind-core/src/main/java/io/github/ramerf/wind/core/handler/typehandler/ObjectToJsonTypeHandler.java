package io.github.ramerf.wind.core.handler.typehandler;

import com.alibaba.fastjson.JSON;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import javax.annotation.Nonnull;

/**
 * 对象转JSON字符串. {@literal java: 对象 <=> jdbc:json string}.
 *
 * @author Tang Xiaofeng
 * @since 2020.12.27
 */
@IgnoreScan
public class ObjectToJsonTypeHandler implements ITypeHandler<Object, String> {

  @Override
  public Object convertToJdbc(
      final Object object, final Field field, @Nonnull final PreparedStatement ps) {
    return JSON.toJSONString(object);
  }

  @Override
  public Object convertFromJdbc(
      final String jdbcVal, final Object defaultValue, final Field field) {
    return JSON.parseObject(jdbcVal, field.getType());
  }
}
