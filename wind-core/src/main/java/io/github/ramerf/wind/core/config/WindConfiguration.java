package io.github.ramerf.wind.core.config;

import lombok.*;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

/**
 * The type Wind configuration.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/14
 */
@Data
@ConfigurationProperties("wind")
public class WindConfiguration {
  /** 逻辑删除字段. */
  private String logicDeleteField = "isDelete";

  /** 逻辑未删除值. */
  private boolean logicNotDelete = false;

  /** 逻辑已删除值. */
  private boolean logicDeleted = true;

  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String entityPackage = "";

  /**
   * 枚举所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String enumPackage = "";

  /** 是否自定义枚举反序列化.设置为true时,可能需要编写枚举反序列化代码. */
  private boolean customEnumDeserializer = false;

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 150;

  /** 雪花分布式id. */
  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();

  /** Redis分布式缓存配置. */
  @NestedConfigurationProperty private RedisCache redisCache = new RedisCache();

  /** Redis 缓存配置. */
  @Setter
  @Getter
  public static class RedisCache {
    /** 是否启用. */
    private boolean enable = true;

    /** 缓存key前缀. */
    private String keyPrefix = "io.github.ramerf.wind";
  }
}
