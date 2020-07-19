package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.util.EntityUtils;

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
 * @author Tang Xiaofeng
 * @since 2019/12/20
 */
public class BaseServiceImpl<T extends AbstractEntityPoJo, R> implements BaseService<T> {
  private final R repository;
  private final BaseService<T> service;

  @Override
  public Class<T> getPoJoClass() {
    return EntityUtils.getPoJoClass(service);
  }

  public BaseServiceImpl(R repository, BaseService<T> service) {
    this.repository = repository;
    this.service = service;
  }

  @Override
  @SuppressWarnings("unchecked")
  public <U> U getRepository() throws RuntimeException {
    return (U) repository;
  }
}
