package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.io.IOException;
import java.util.Objects;

/**
 * 定义枚举的序列化.
 *
 * @author ramer
 * @see InterEnumSerializer#serializer(InterEnum)
 * @see InterEnumSerializer#defaultSerializer(InterEnum) .
 */
public class JacksonEnumSerializer extends JsonSerializer<InterEnum> {
  @Override
  public void serialize(
      InterEnum interEnum, JsonGenerator jsonGenerator, SerializerProvider serializerProvider)
      throws IOException {
    Object object;
    InterEnumSerializer interEnumSerializer;
    if (Objects.nonNull(
        interEnumSerializer = AppContextInject.getBean(InterEnumSerializer.class))) {
      object = interEnumSerializer.serializer(interEnum);
    } else {
      interEnumSerializer = obj -> null;
      object = interEnumSerializer.defaultSerializer(interEnum);
    }
    jsonGenerator.writeObject(object);
  }
}
