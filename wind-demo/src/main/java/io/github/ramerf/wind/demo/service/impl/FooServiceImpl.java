package io.github.ramerf.wind.demo.service.impl;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.repository.FooRepository;
import io.github.ramerf.wind.demo.service.FooService;
import java.util.function.Consumer;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Slf4j
@Service
public class FooServiceImpl implements FooService {
  @Resource private FooRepository repository;

  @Resource(name = "fooBs")
  private BaseService<Foo> baseService;

  @Override
  public Page<Foo> page(
      Consumer<QueryColumn<Foo>> consumer, int page, int size, SortColumn sortColumn) {
    final Page<Foo> poJoPage = baseService.page(consumer, page, size, sortColumn);
    log.info("page:这里可以执行额外操作[{}]", poJoPage);
    return poJoPage;
  }

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
    update(poJo);
  }

  @Override
  public void deleteNoResult() {}

  @Override
  public int deleteWithResult() {
    return 0;
  }

  @Bean("fooBs")
  public BaseService<Foo> setBaseService(@Autowired FooRepository repository) {
    return new BaseServiceImpl<>(repository);
  }

  @SuppressWarnings({"unchecked"})
  @Override
  public <U> U getRepository() throws CommonException {
    return (U) repository;
  }
}
