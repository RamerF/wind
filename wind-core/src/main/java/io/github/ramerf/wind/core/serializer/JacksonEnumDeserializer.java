package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.EnumUtils;
import io.github.ramerf.wind.core.validation.InterEnumConstraint;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.*;
import lombok.extern.slf4j.Slf4j;

/**
 * Jackson枚举反序列化.格式:{value:"值",desc:"描述"}.
 *
 * @param <T> the type parameter
 * @author ramer
 */
@Slf4j
@SuppressWarnings("rawtypes")
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
    // 匹配值
    if (jsonToken == JsonToken.VALUE_NUMBER_INT) {
      return deserializeValue(parser);
    }
    // 匹配值名或名称
    if (jsonToken == JsonToken.VALUE_STRING) {
      return deserializeName(parser);
    }
    log.warn("deserialize:cannot deserialize text[{}]", parser.getText());
    return null;
  }

  private String getErrorMessage(final JsonParser parser) {
    Object entity = parser.getCurrentValue();
    String prop;
    try {
      prop = parser.getCurrentName();
      Field field = Objects.requireNonNull(BeanUtils.getDeclaredField(entity.getClass(), prop));
      InterEnumConstraint annotation = field.getAnnotation(InterEnumConstraint.class);
      if (annotation != null) {
        return annotation.message();
      }
    } catch (IOException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return null;
  }

  @SuppressWarnings({"unchecked", "UnusedAssignment"})
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
    return (T) EnumUtils.of(value, clazz);
  }

  @SuppressWarnings("unchecked")
  private T deserializeValue(final JsonParser parser) throws IOException {
    String text = parser.getText();
    if (text == null) {
      return null;
    }
    return (T) InterEnum.of(text, clazz, () -> getErrorMessage(parser));
  }

  @SuppressWarnings("unchecked")
  private T deserializeName(final JsonParser parser) throws IOException {
    final String text = parser.getText();
    if (text == null) {
      return null;
    }
    Optional<T> optional =
        Arrays.stream(clazz.getEnumConstants())
            .filter(o -> Objects.equals(o.toString(), text))
            .findFirst();
    //noinspection OptionalIsPresent
    return optional.isPresent()
        ? optional.get()
        : (T) InterEnum.of(text, clazz, () -> getErrorMessage(parser));
  }
}
