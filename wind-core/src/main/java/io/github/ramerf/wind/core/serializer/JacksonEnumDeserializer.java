package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.util.EnumUtils;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;

/**
 * Jackson 枚举反序列化,格式: {value: "值",desc: "描述"}.
 *
 * @param <T> the type parameter
 * @author ramer
 */
@Slf4j
public class JacksonEnumDeserializer<T extends InterEnum> extends JsonDeserializer<T> {
  private final Class<T> clazz;

  /**
   * Instantiates a new Jackson enum deserializer.
   *
   * @param clazz the clazz
   */
  public JacksonEnumDeserializer(Class<T> clazz) {
    this.clazz = clazz;
  }

  @Override
  public T deserialize(JsonParser parser, DeserializationContext context) throws IOException {
    JsonToken jsonToken = parser.currentToken();
    // 格式: {"value":"值","desc":"描述"}
    if (jsonToken == JsonToken.START_OBJECT) {
      jsonToken = parser.nextToken();
      return deserializeKeyVal(parser, jsonToken);
    }
    // 仅返回值
    if (jsonToken == JsonToken.VALUE_NUMBER_INT) {
      return deserializeValue(parser);
    }
    // 仅返回枚举名称
    if (jsonToken == JsonToken.VALUE_STRING) {
      return deserializeName(parser);
    }
    log.warn("deserialize:cannot deserialize text[{}]", parser.getText());
    return null;
  }

  private T deserializeKeyVal(final JsonParser parser, JsonToken jsonToken) throws IOException {
    int value = 0;
    while (jsonToken == JsonToken.FIELD_NAME) {
      final String fieldName = parser.currentName();
      jsonToken = parser.nextToken();
      if ("value".equals(fieldName)) {
        value = parser.getIntValue();
      }
      jsonToken = parser.nextToken();
    }
    return EnumUtils.of(clazz, value);
  }

  private T deserializeValue(final JsonParser p) throws IOException {
    return EnumUtils.of(clazz, Integer.valueOf(p.getText()));
  }

  private T deserializeName(final JsonParser p) throws IOException {
    return Arrays.stream(clazz.getEnumConstants())
        .filter(
            o -> {
              try {
                return Objects.equals(o.toString(), p.getText());
              } catch (IOException e) {
                log.warn(e.getMessage());
                log.error(e.getMessage(), e);
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }
}
