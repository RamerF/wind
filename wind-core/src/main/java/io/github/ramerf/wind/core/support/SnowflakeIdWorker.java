package io.github.ramerf.wind.core.support;

/**
 * 雪花分布式id生成算法.
 *
 * @author Tang Xiaofeng
 * @since 2019/11/12
 */
@SuppressWarnings("all")
public class SnowflakeIdWorker {
  /** 开始时间截 (2020-05-20 00:00:00) */
  private final long startEpoch = 1589904000000L;

  /** 机器id所占的位数 */
  private final long workerIdBits = 4L;

  /** 数据标识id所占的位数 */
  private final long datacenterIdBits = 4L;

  /** 支持的最大机器id，结果是31 (这个移位算法可以很快的计算出几位二进制数所能表示的最大十进制数) */
  private final long maxWorkerId = ~(-1L << workerIdBits);

  /** 支持的最大数据标识id，结果是31 */
  private final long maxDatacenterId = ~(-1L << datacenterIdBits);

  /** 序列在id中占的位数 */
  private final long sequenceBits = 5L;

  /** 机器ID向左移12位 */
  private final long workerIdShift = sequenceBits;

  /** 数据标识id向左移17位(12+5) */
  private final long datacenterIdShift = sequenceBits + workerIdBits;

  /** 时间截向左移22位(5+5+12) */
  private final long timestampLeftShift = sequenceBits + workerIdBits + datacenterIdBits;

  /** 生成序列的掩码，这里为4095 (0b111111111111=0xfff=4095) */
  private final long sequenceMask = ~(-1L << sequenceBits);

  /** 工作机器ID(0~31) */
  public static long workerId;

  /** 数据中心ID(0~31) */
  public static long datacenterId;

  /** 毫秒内序列(0~4095) */
  private long sequence = 0L;

  /** 上次生成ID的时间截 */
  private long lastTimestamp = -1L;

  public SnowflakeIdWorker() {}
  /**
   * 构造函数
   *
   * @param workerId 工作ID (0~31)
   * @param dataCenterId 数据中心ID (0~31)
   */
  public SnowflakeIdWorker(long workerId, long dataCenterId) {
    if (workerId > maxWorkerId || workerId < 0) {
      throw new IllegalArgumentException(
          String.format("worker Id can't be greater than %d or less than 0", maxWorkerId));
    }
    if (dataCenterId > maxDatacenterId || dataCenterId < 0) {
      throw new IllegalArgumentException(
          String.format("datacenter Id can't be greater than %d or less than 0", maxDatacenterId));
    }
    SnowflakeIdWorker.workerId = workerId;
    SnowflakeIdWorker.datacenterId = dataCenterId;
  }

  public static void setWorkerId(final long workerId) {
    SnowflakeIdWorker.workerId = workerId;
  }

  public static void setDatacenterId(final long datacenterId) {
    SnowflakeIdWorker.datacenterId = datacenterId;
  }

  // ==============================Methods==========================================
  /**
   * 获得下一个ID (该方法是线程安全的)
   *
   * @return SnowflakeId
   */
  public synchronized long nextId() {
    long timestamp = timeGen();

    // 如果当前时间小于上一次ID生成的时间戳，说明系统时钟回退过这个时候应当抛出异常
    if (timestamp < lastTimestamp) {
      throw new RuntimeException(
          String.format(
              "Clock moved backwards.  Refusing to generate id for %d milliseconds",
              lastTimestamp - timestamp));
    }

    // 如果是同一时间生成的，则进行毫秒内序列
    if (lastTimestamp == timestamp) {
      sequence = (sequence + 1) & sequenceMask;
      // 毫秒内序列溢出
      if (sequence == 0) {
        // 阻塞到下一个毫秒,获得新的时间戳
        timestamp = tilNextMillis(lastTimestamp);
      }
    }
    // 时间戳改变，毫秒内序列重置
    else {
      sequence = 0;
    }

    // 上次生成ID的时间截
    lastTimestamp = timestamp;

    // 移位并通过或运算拼到一起组成64位的ID
    return ((timestamp - startEpoch) << timestampLeftShift) //
        | (datacenterId << datacenterIdShift) //
        | (workerId << workerIdShift) //
        | sequence;
  }

  /**
   * 阻塞到下一个毫秒，直到获得新的时间戳
   *
   * @param lastTimestamp 上次生成ID的时间截
   * @return 当前时间戳
   */
  protected long tilNextMillis(long lastTimestamp) {
    long timestamp = timeGen();
    while (timestamp <= lastTimestamp) {
      timestamp = timeGen();
    }
    return timestamp;
  }

  /**
   * 返回以毫秒为单位的当前时间
   *
   * @return 当前时间(毫秒)
   */
  protected long timeGen() {
    return System.currentTimeMillis();
  }

  // ==============================Test=============================================
  /**
   * 测试 .
   *
   * @param args the args
   */
  public static void main(String[] args) {
    SnowflakeIdWorker idWorker = new SnowflakeIdWorker(0, 0);
    for (int i = 0; i < 1000; i++) {
      long id = idWorker.nextId();
      System.out.println(Long.toBinaryString(id));
      System.out.println(id);
    }
  }
}