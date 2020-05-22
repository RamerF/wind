package io.github.ramerf.wind.core.support;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.stereotype.Component;

/**
 * id生成策略.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/20
 */
@Component
@ConditionalOnMissingBean(IdGenerator.class)
public class SnowflakeIdGenerator implements IdGenerator {
  private final SnowflakeIdWorker snowflakeIdWorker = new SnowflakeIdWorker();

  @Override
  public Long nextId(final Object obj) {
    return snowflakeIdWorker.nextId();
  }
}
