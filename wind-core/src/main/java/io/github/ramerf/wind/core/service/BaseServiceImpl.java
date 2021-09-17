package io.github.ramerf.wind.core.service;

import java.io.Serializable;

/**
 * 该类用于service覆写方法后,调用原方法,service代码示例:
 *
 * <pre>
 *   // 定义bean
 *   <code>@Bean("fooBs")</code>
 *   public BaseService&lt;Foo&gt; setBaseService(@Autowired FooRepository repository) {
 *     return new BaseServiceImpl&lt;&gt;(repository, this);
 *   }
 *
 *   // 注入
 *   <code>@Resource(name = "fooBs")</code>
 *   private BaseService&lt;Foo&gt; baseService;
 *
 *   // 通过baseService调用原方法
 *   baseService.create(foo);
 * </pre>
 *
 * @author ramer
 * @since 2019/12/20
 */
public class BaseServiceImpl<T, ID extends Serializable, R> implements BaseService<T, ID> {
  private final R repository;
  private final BaseService<T, ID> service;

  public BaseServiceImpl(R repository, BaseService<T, ID> service) {
    this.repository = repository;
    this.service = service;
  }
}
