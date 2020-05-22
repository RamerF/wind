package io.github.ramerf.wind.core.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;

/**
 * 定义枚举的序列化.
 *
 * @author ramer
 * @see InterEnumSerializer#serializer(InterEnum)
 * @see InterEnumSerializer#defaultSerializer(InterEnum) .
 */
@Slf4j
public class JacksonEnumSerializer extends JsonSerializer<InterEnum> {
  @Override
  public void serialize(
      InterEnum interEnum, JsonGenerator jsonGenerator, SerializerProvider serializerProvider)
      throws IOException {
    Object object;
    InterEnumSerializer interEnumSerializer;
    try {
      interEnumSerializer = AppContextInject.getBean(InterEnumSerializer.class);
      object = interEnumSerializer.serializer(interEnum);
    } catch (BeansException e) {
      log.debug("serialize:[No InterEnumSerializer defined,use default.]");
      interEnumSerializer = obj -> null;
      object = interEnumSerializer.defaultSerializer(interEnum);
    }
    jsonGenerator.writeObject(object);
  }
}
