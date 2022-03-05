package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.executor.SimpleJdbcExecutor;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.plugin.Interceptor;
import io.github.ramerf.wind.core.plugin.InterceptorChain;
import io.github.ramerf.wind.core.support.IdGenerator;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * wind配置.
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

  /** 批量操作时,每次处理的大小. */
  protected int batchSize = 500;

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

  /** 拦截器 */
  protected final InterceptorChain interceptorChain = new InterceptorChain();

  public void addInterceptor(Interceptor interceptor) {
    interceptorChain.addInterceptor(interceptor);
  }

  public Executor newExecutor(Transaction transaction) {
    Executor executor = new SimpleJdbcExecutor(this, transaction);
    return (Executor) interceptorChain.pluginAll(executor);
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
