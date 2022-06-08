package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.annotation.UpdateTimestamp;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.plugin.*;
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

  /** entity所在包路径,多个以,分割. */
  protected String entityPackage = "";

  /** 拦截器所在包路径,多个以,分割. */
  protected String interceptorPackage = "";

  /** 类型处理器路径,多个以,分割 */
  protected String typeHandlerPackage = "";

  /** 批量操作时,每次处理的大小. */
  protected int batchSize = 500;

  /** 表更新模式. */
  protected DdlAuto ddlAuto = DdlAuto.NONE;

  /** 数据库方言全路径. */
  protected String dialect;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  protected boolean writeNullProp = true;

  /** 指定{@link UpdateTimestamp}注解的更新策略,默认总是赋值为当前时间 */
  protected TimestampStrategy updateTimeStrategy = TimestampStrategy.ALWAYS;

  /** 全局id生成器,默认自增,实体可以单独指定{@link TableInfo#idGenerator()} */
  protected IdGenerator idGenerator = IdGenerator.AUTO_INCREMENT_ID_GENERATOR;

  /** jdbc环境配置.数据源,事务 */
  protected JdbcEnvironment jdbcEnvironment;

  /** dao拦截器 */
  protected final DaoInterceptorChain daoInterceptorChain = new DaoInterceptorChain();
  /** service拦截器 */
  protected final ServiceInterceptorChain serviceInterceptorChain = new ServiceInterceptorChain();

  /** 添加dao拦截器. */
  public void addInterceptor(DaoInterceptor daoInterceptor) {
    daoInterceptorChain.addInterceptor(daoInterceptor);
  }

  /** 添加拦截器. */
  public void addInterceptor(ServiceInterceptor serviceInterceptor) {
    serviceInterceptorChain.addInterceptor(serviceInterceptor);
  }

  public void addTypeHandler(ITypeHandler<?, ?>... typeHandler) {
    TypeHandlerRegistryFactory.addTypeHandlers(typeHandler);
  }

  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }

  public enum TimestampStrategy {
    /** 总是设置为当前时间 */
    ALWAYS,
    /** 仅字段为空时设置为当前时间 */
    NULL
  }
}
