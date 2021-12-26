package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.BeanUtils;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

/**
 * The type Wind configuration.
 *
 * @since 2020 /1/14
 * @author ramer
 */
@Data
@Slf4j
public class Configuration {

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

  /** 表更新模式. */
  private DdlAuto ddlAuto = DdlAuto.NONE;

  /** 数据库方言全路径. */
  private String dialect;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  private boolean writeNullProp = true;

  /** 全局id生成器,实体可以单独指定{@link TableInfo#idGenerator()} */
  @Getter(AccessLevel.NONE)
  private String idGenerator;

  public IdGenerator getIdGenerator() {
    return idGenerator != null
        ? BeanUtils.initial(this.idGenerator)
        : IdGenerator.VOID_ID_GENERATOR;
  }

  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }
}
