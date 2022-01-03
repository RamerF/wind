package io.github.ramerf.wind.web.config;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.serializer.InterEnumSerializer;
import io.github.ramerf.wind.core.serializer.JacksonEnumSerializer;
import io.github.ramerf.wind.core.support.StringToEnumConverterFactory;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 定义常用bean.
 *
 * @author ramer
 * @since 2019 /12/29
 */
@Slf4j
@Configuration("wind_web_common_bean")
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE + 10)
public class CommonBean {

  /**
   * String to enum converter factory mvc configure web mvc configurer.
   *
   * @return the web mvc configurer
   */
  @Bean
  @ConditionalOnProperty(
      value = "wind.enable-web-mvc-configurer",
      havingValue = "true",
      matchIfMissing = true)
  public WebMvcConfigurer stringToEnumConverterFactoryMvcConfigure() {
    // 添加枚举转换器,请求可以传递value整型值
    return new WebMvcConfigurer() {
      @Override
      public void addFormatters(@Nonnull FormatterRegistry registry) {
        registry.addConverterFactory(new StringToEnumConverterFactory());
      }

      // @Override
      // public void addCorsMappings(@Nonnull CorsRegistry registry) {
      //   // TODO WARN 这个跨域配置有问题
      //   final long maxAge = 3600L;
      //   registry
      //       .addMapping("/**")
      //       .allowedOrigins("*")
      //       .allowedMethods("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "TRACE")
      //       .allowCredentials(false)
      //       .maxAge(maxAge);
      // }
    };
  }

  /**
   * Jackson object mapper customizer jackson 2 object mapper builder customizer.
   *
   * @return the jackson 2 object mapper builder customizer
   */
  @Bean
  public Jackson2ObjectMapperBuilderCustomizer jacksonObjectMapperCustomizer(
      ObjectProvider<InterEnumSerializer> interEnumSerializer) {
    return objectMapperBuilder ->
        objectMapperBuilder.serializerByType(
            InterEnum.class, new JacksonEnumSerializer(interEnumSerializer.getIfAvailable()));
  }
}
