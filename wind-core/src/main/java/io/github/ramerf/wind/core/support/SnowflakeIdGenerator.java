package io.github.ramerf.wind.core.support;

/**
 * id生成策略.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/20
 */
public class SnowflakeIdGenerator implements IdGenerator {
  private final SnowflakeIdWorker snowflakeIdWorker = new SnowflakeIdWorker();

  @Override
  public Long nextId(final Object obj) {
    return snowflakeIdWorker.nextId();
  }
}
