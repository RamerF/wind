package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;

/**
 * 向前端返回时将Long转成字符串.
 *
 * @author Tang Xiaofeng
 * @since 2020/1/17
 */
public class LongJsonSerializer extends JsonSerializer<Long> {
  @Override
  public void serialize(
      Long value, JsonGenerator jsonGenerator, SerializerProvider serializerProvider)
      throws IOException {
    String text = (value == null ? null : String.valueOf(value));
    if (text != null) {
      jsonGenerator.writeString(text);
    }
  }
}
