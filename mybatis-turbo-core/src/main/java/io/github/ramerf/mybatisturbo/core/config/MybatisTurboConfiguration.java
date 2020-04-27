package io.github.ramerf.mybatisturbo.core.config;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
@Data
@Configuration
public class MybatisTurboConfiguration {

  @Value("${mybatis-plus.global-config.db-config.service-delete-field:isDelete}")
  private String logicDeleteField;

  @Value("${mybatis-plus.global-config.db-config.service-not-delete-value:false}")
  private boolean logicNotDelete;

  @Value("${mybatis-plus.global-config.db-config.service-delete-value:true}")
  private boolean logicDeleted;
}
