package io.github.ramerf.wind.demo.service.impl;

import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.repository.FooRepository;
import io.github.ramerf.wind.demo.service.FooService;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author ramer
 * @since 2019/12/17
 */
@SuppressWarnings("unchecked")
@Slf4j
@Service
public class FooServiceImpl implements FooService {
  @Resource private FooRepository repository;

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
}
