package io.github.ramerf.wind.web.support;

import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.parser.*;
import com.alibaba.fastjson.parser.deserializer.ObjectDeserializer;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.lang.reflect.Type;
import java.util.Objects;

@SuppressWarnings({"rawtypes", "unchecked"})
public class EnumValueDeserializer implements ObjectDeserializer {
  @Override
  public <T> T deserialze(DefaultJSONParser parser, Type type, Object fieldName) {
    final JSONLexer lexer = parser.lexer;
    final int token = lexer.token();
    Class cls = (Class) type;
    Object[] enumConstants = cls.getEnumConstants();
    if (InterEnum.class.isAssignableFrom(cls)) {
      for (Object enumConstant : enumConstants) {
        if (Objects.equals(((InterEnum) enumConstant).value(), lexer.intValue())) {
          return (T) enumConstant;
        }
      }
    } else {
      // 没实现InterEnum接口的 默认的按名字或者按ordinal
      if (token == JSONToken.LITERAL_INT) {
        int intValue = lexer.intValue();
        lexer.nextToken(JSONToken.COMMA);

        if (intValue < 0 || intValue > enumConstants.length) {
          throw new JSONException("parse enum " + cls.getName() + " error, value : " + intValue);
        }
        return (T) enumConstants[intValue];
      } else if (token == JSONToken.LITERAL_STRING) {
        return (T) Enum.valueOf(cls, lexer.stringVal());
      }
    }
    // 对象参数中包含枚举字段,值无效时
    if (token == JSONToken.LITERAL_INT) {
      return null;
    }
    if (token == JSONToken.LITERAL_STRING) {
      return null;
    }
    return null;
  }

  @Override
  public int getFastMatchToken() {
    return JSONToken.LITERAL_INT;
  }
}
