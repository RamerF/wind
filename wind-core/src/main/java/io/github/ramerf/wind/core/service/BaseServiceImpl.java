package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.executor.Dao;
import java.io.Serializable;

/**
 * 预留实现.
 *
 * @author ramer
 * @since 2019/12/20
 */
public abstract class BaseServiceImpl<T, ID extends Serializable> implements BaseService<T, ID> {
  private final Dao dao;

  public BaseServiceImpl(final Dao dao) {
    this.dao = dao;
  }

  @Override
  public Dao getDao() {
    // 扫描service实现类,如果有方法包含数据源时,代理一下,去configuration获取指定的,否则使用默认的

    // 获取当前线程绑定的数据源 TransactionSynchronizationManager.getConnection(  );
    // 先把dao层的动态代理demo了
    // TODO WARN 这里可以传数据源,否则使用默认数据源
    return dao;
  }
  // TODO WARN FooServiceImpl fooServiceImpl = new FooServiceImpl(dao);

  // TODO WARN 需要一个辅助代理service的东西
  // FooServiceImpl fooServiceImpl = ServiceManager.getService(FooServiceImpl.class,dao);
}
