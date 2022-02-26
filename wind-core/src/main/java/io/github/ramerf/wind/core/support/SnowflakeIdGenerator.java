package io.github.ramerf.wind.core.support;

/**
 * id生成策略.
 *
 * @author ramer
 * @since 2020 /5/20
 */
public class SnowflakeIdGenerator implements IdGenerator {
  private static int workerId = 1;
  private static int dataCenterId = 1;
  private SnowflakeIdWorker snowflakeIdWorker = new SnowflakeIdWorker(workerId, dataCenterId);

  public void initial(final int workerId, final int dataCenterId) {
    SnowflakeIdGenerator.workerId = workerId;
    SnowflakeIdGenerator.dataCenterId = dataCenterId;
    snowflakeIdWorker = new SnowflakeIdWorker(workerId, dataCenterId);
  }

  @Override
  public Object nextId(final Object obj) {
    return snowflakeIdWorker.nextId();
  }
}
