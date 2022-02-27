package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import io.github.ramerf.wind.core.domain.InterEnum;
import java.io.IOException;
import lombok.extern.slf4j.Slf4j;

/**
 * 定义枚举的序列化.
 *
 * @author ramer
 * @see InterEnumSerializer#serializer(InterEnum)
 */
@Slf4j
public class JacksonEnumSerializer extends JsonSerializer<InterEnum<?>> {
  final InterEnumSerializer interEnumSerializer;

  public JacksonEnumSerializer(final InterEnumSerializer interEnumSerializer) {
    this.interEnumSerializer = interEnumSerializer;
  }

  @Override
  public void serialize(
      InterEnum interEnum, JsonGenerator jsonGenerator, SerializerProvider serializerProvider)
      throws IOException {
    jsonGenerator.writeObject(
        interEnumSerializer != null
            ? interEnumSerializer.serializer(interEnum)
            : interEnum.value());
  }
}
