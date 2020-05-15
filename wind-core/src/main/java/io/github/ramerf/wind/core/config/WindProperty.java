package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.util.SnowflakeIdWorker;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Configuration;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
@Setter
@Getter
@Configuration
public class WindProperty {
  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  @Value("${wind.entity-package:}")
  private String entityPackage;
  /** 用于分布式主键生成{@link SnowflakeIdWorker#setWorkerId(long)}. */
  @Value("${wind.worker-id:0}")
  private Long workId;

  /** 用于分布式主键生成.{@link SnowflakeIdWorker#setDatacenterId(long)} */
  @Value("${wind.data-center-id:0}")
  private Long dataCenterId;
}
