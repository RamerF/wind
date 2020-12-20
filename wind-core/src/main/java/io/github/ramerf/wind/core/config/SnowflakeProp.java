package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.support.SnowflakeIdWorker;
import lombok.Getter;
import lombok.Setter;

/**
 * 雪花分布式id生成器属性.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/16
 */
@Setter
@Getter
public class SnowflakeProp {
  /** 用于分布式主键生成{@link SnowflakeIdWorker#workerId}. */
  private Long workerId = 0L;

  /** 用于分布式主键生成.{@link SnowflakeIdWorker#datacenterId} */
  private Long dataCenterId = 0L;
}
