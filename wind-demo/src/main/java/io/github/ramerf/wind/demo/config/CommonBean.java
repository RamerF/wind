package io.github.ramerf.wind.demo.config;

import com.alibaba.fastjson.JSONObject;
import io.github.ramerf.wind.core.serializer.InterEnumSerializer;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.support.SnowflakeIdGenerator;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.*;
import springfox.documentation.schema.ModelRef;
import springfox.documentation.service.*;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * 通用bean定义.
 *
 * @author ramer
 * @since 2019 /12/6
 */
@Slf4j
@EnableSwagger2
@Configuration("config_common_bean")
public class CommonBean {
  /** id 使用雪花算法. */
  @Bean
  public IdGenerator autoIncrementGenerator() {
    return new SnowflakeIdGenerator();
  }

  /**
   * 自定义枚举的序列化格式.
   *
   * @return the inter enum serializer
   */
  @Bean
  public InterEnumSerializer interEnumSerializer() {
    return interEnum -> {
      JSONObject jsonObject = new JSONObject();
      jsonObject.put("key", interEnum.value());
      jsonObject.put("value", interEnum.desc());
      return jsonObject;
    };
  }

  /**
   * Api docket.
   *
   * @return the docket
   */
  @Bean
  @ConditionalOnProperty(value = "spring.swagger.enable", havingValue = "true")
  public Docket api() {
    ParameterBuilder parameterBuilder = new ParameterBuilder();
    List<Parameter> parameters = new ArrayList<>();
    parameterBuilder
        .name("Authorization")
        .description("令牌")
        .modelRef(new ModelRef("string"))
        .parameterType("header")
        .required(false)
        .build();
    parameters.add(parameterBuilder.build());
    return new Docket(DocumentationType.SWAGGER_2)
        .apiInfo(apiInfo())
        .enable(true)
        .select()
        .apis(RequestHandlerSelectors.basePackage("io.github.ramerf.wind.demo"))
        .paths(PathSelectors.any())
        .build()
        .globalOperationParameters(parameters);
  }

  private ApiInfo apiInfo() {
    return new ApiInfoBuilder()
        .title("项目接口文档")
        .description("项目描述")
        .contact(new Contact("ramer", "https://github.com/ramerf/wind.git", "1390635973@qq.com"))
        .version("1.0.0")
        .build();
  }
}
