package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.util.SnowflakeIdWorker;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;

/**
 * 雪花分布式id生成器属性.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/16
 */
@Setter
@Getter
public class SnowflakeProp {
  /** 用于分布式主键生成{@link SnowflakeIdWorker#setWorkerId(long)}. */
  @Value("${wind.snowflake-prop.worker-id:0}")
  private Long workId;

  /** 用于分布式主键生成.{@link SnowflakeIdWorker#setDatacenterId(long)} */
  @Value("${wind.snowflake-prop.data-center-id:0}")
  private Long dataCenterId;
}
