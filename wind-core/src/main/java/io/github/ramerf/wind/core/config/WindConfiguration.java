package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.util.List;
import lombok.*;
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

  /** 是否启用通用mvc配置. */
  private boolean enableWebMvcConfigurer = true;

  // /** 过滤创建/更新时间字段.  */
  // private List<String> excludeTimeFields;

  /** 自动更新表模式. */
  private DdlAuto ddlAuto;

  /** 数据库方言全路径. */
  private String dialect;

  /** 禁用{@link AbstractEntityPoJo}中的公共字段.可选值: createTime,updateTime */
  private List<String> disableFields;

  /** 雪花分布式id. */
  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();

  /** Redis分布式缓存配置. */
  @NestedConfigurationProperty private RedisCache redisCache = new RedisCache();

  /**
   * Redis 缓存配置. @since 2020.08.23
   *
   * @author Tang Xiaofeng
   */
  @Setter
  @Getter
  public static class RedisCache {
    /** 是否启用. */
    private boolean enable = true;

    /** 缓存key前缀. */
    private String keyPrefix = "io.github.ramerf.wind";
  }

  /** The enum Ddl auto. @author Tang Xiaofeng */
  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }
}
