package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.plugin.InterceptorChain;

/**
 * The type Configuration.
 *
 * @since 2020/1/14
 * @author ramer
 */
@Data
@Slf4j
public class Configuration {

  /** 逻辑删除配置. */
  protected LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** entity所在包路径,多个以,分割.<br> */
  protected String entityPackage = "";

  /** 枚举所在包路径,多个以,分割.<br> */
  protected String enumPackage = "";

  /** 是否自定义枚举反序列化.设置为true时,可能需要编写枚举反序列化代码. */
  protected boolean customEnumDeserializer = false;

  /** 批量操作时,每次处理的大小. */
  protected int batchSize = 150;

  /** 是否启用默认mvc配置. */
  protected boolean enableWebMvcConfigurer = true;

  /** 表更新模式. */
  protected DdlAuto ddlAuto = DdlAuto.NONE;

  /** 数据库方言全路径. */
  protected String dialect;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  protected boolean writeNullProp = true;

  /** 全局id生成器,默认自增,实体可以单独指定{@link TableInfo#idGenerator()} */
  protected IdGenerator idGenerator = IdGenerator.AUTO_INCREMENT_ID_GENERATOR;

  /** jdbc环境配置.数据源,事务 */
  protected JdbcEnvironment jdbcEnvironment;

  protected final InterceptorChain interceptorChain = new InterceptorChain();

  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }
}
