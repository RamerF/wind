package io.github.ramerf.wind.core.support;

/**
 * id生成策略.
 *
 * @author ramer
 * @since 2020 /5/20
 */
public class SnowflakeIdGenerator implements IdGenerator {
  private final SnowflakeIdWorker snowflakeIdWorker = new SnowflakeIdWorker(1, 1);

  @Override
  public Object nextId(final Object obj) {
    return snowflakeIdWorker.nextId();
  }
}
