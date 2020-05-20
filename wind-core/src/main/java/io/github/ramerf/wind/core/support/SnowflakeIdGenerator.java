package io.github.ramerf.wind.core.support;

import org.springframework.stereotype.Component;

/**
 * id生成策略.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/20
 */
@Component
public class SnowflakeIdGenerator implements IdGenerator {

  @Override
  public Long nextId(final Object obj) {
    return new SnowflakeIdWorker().nextId();
  }
}
