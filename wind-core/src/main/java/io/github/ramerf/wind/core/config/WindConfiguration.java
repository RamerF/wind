package io.github.ramerf.wind.core.config;

import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

/**
 * The type Wind configuration.
 *
 * @since 2020 /1/14
 * @author Tang Xiaofeng
 */
@Data
@Slf4j
@ConfigurationProperties("wind")
public class WindConfiguration {

  /** 逻辑删除配置. */
  @NestedConfigurationProperty private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

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

  /** 是否启用默认mvc配置. */
  private boolean enableWebMvcConfigurer = true;

  /** 自动更新表模式. */
  private DdlAuto ddlAuto;

  /** 数据库方言全路径. */
  private String dialect;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  private boolean writeNullProp = true;

  /** 雪花分布式id. */
  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();

  /** 缓存配置. */
  @NestedConfigurationProperty private CacheConfig cache = new CacheConfig();

  /**
   * Redis 缓存配置.
   *
   * @since 2020.08.23
   * @author Tang Xiaofeng
   */
  @Setter
  @Getter
  public static class CacheConfig {
    /** 是否启用. */
    private CacheType type = CacheType.MEMORY;

    /** 缓存key前缀. */
    private String keyPrefix = "io.github.ramerf.wind";
  }

  public enum CacheType {
    /** none cache. */
    NONE,
    /** redis. */
    REDIS,
    /** memory. */
    MEMORY,
  }

  /**
   * The enum Ddl auto.
   *
   * @author Tang Xiaofeng
   */
  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }
}
