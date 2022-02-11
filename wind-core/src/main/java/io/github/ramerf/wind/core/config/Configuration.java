package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

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
  private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** entity所在包路径,多个以,分割.<br> */
  private String entityPackage = "";

  /** 枚举所在包路径,多个以,分割.<br> */
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

  /** 全局id生成器,默认自增,实体可以单独指定{@link TableInfo#idGenerator()} */
  private IdGenerator idGenerator = IdGenerator.AUTO_INCREMENT_ID_GENERATOR;

  /** jdbc环境配置.数据源,事务 */
  private JdbcEnvironment jdbcEnvironment;

  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }
}
