package io.github.ramerf.mybatisturbo.core.support;

import com.alibaba.fastjson.serializer.*;
import io.github.ramerf.mybatisturbo.core.entity.enums.InterEnum;
import java.lang.reflect.Type;

public class EnumValueSerializer implements ObjectSerializer {
  @Override
  public void write(
      JSONSerializer serializer, Object object, Object fieldName, Type fieldType, int features) {
    SerializeWriter serializeWriter = serializer.out;
    if (object instanceof InterEnum) {
      InterEnum interEnum = (InterEnum) object;
      serializeWriter.write(String.valueOf(interEnum.value()));
    } else {
      serializeWriter.writeEnum((Enum<?>) object);
    }
  }
}
