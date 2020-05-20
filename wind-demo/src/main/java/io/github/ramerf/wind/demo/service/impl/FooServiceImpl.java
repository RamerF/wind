package io.github.ramerf.wind.demo.service.impl;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.repository.FooRepository;
import io.github.ramerf.wind.demo.service.FooService;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@SuppressWarnings("unchecked")
@Slf4j
@Service
public class FooServiceImpl implements FooService {
  @Resource private FooRepository repository;

  @Resource(name = "fooBs")
  private BaseService<Foo> baseService;
  //
  //  @Override
  //  public long create(@Nonnull Foo foo, IFunction<Foo, ?>... includeNullProps)
  //      throws RuntimeException {
  //    log.info("create:[{}]", foo);
  //    final long id = baseService.create(foo, includeNullProps);
  //    log.info("create:这里可以执行额外操作[{}]", id);
  //    return id;
  //  }

  @Override
  public Foo enableCache(final long id) {
    return getById(id);
  }

  @Cacheable(
      value = "io.github.ramerf.wind.demo.service.impl.FooServiceImpl",
      key = "caches[0].name+'('+#id+')'")
  @Override
  public Foo redisCache(final long id) {
    return getById(id);
  }

  @Override
  @CacheEvict(
      value = "io.github.ramerf.wind.demo.service.impl.FooServiceImpl",
      key = "caches[0].name+'('+#poJo.id+')'")
  @Transactional(rollbackFor = Exception.class)
  public void redisCacheClear(final Foo poJo) {
    final int update = update(poJo);
    log.info("redisCacheClear:[{}]", update);
  }

  @Override
  public void deleteNoResult() {}

  @Override
  public int deleteWithResult() {
    return 0;
  }

  @Bean("fooBs")
  public BaseService<Foo> setBaseService(@Autowired FooRepository repository) {
    return new BaseServiceImpl<>(repository, this);
  }

  @SuppressWarnings({"unchecked"})
  @Override
  public <U> U getRepository() throws CommonException {
    return (U) repository;
  }
}
