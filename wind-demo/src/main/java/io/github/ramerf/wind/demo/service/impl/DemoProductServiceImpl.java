package io.github.ramerf.wind.demo.service.impl;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.demo.repository.DemoProductRepository;
import io.github.ramerf.wind.demo.service.DemoProductService;
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
public class DemoProductServiceImpl implements DemoProductService {
  @Resource private DemoProductRepository repository;

  @Resource(name = "demoProductBs")
  private BaseService<DemoProductPoJo> baseService;

  @Override
  public Page<DemoProductPoJo> page(
      Consumer<QueryColumn<DemoProductPoJo>> consumer, int page, int size, SortColumn sortColumn) {
    final Page<DemoProductPoJo> poJoPage = baseService.page(consumer, page, size, sortColumn);
    log.info("page:这里可以执行额外操作[{}]", poJoPage);
    return poJoPage;
  }

  @Override
  public DemoProductPoJo enableCache(final long id) {
    return getById(id);
  }

  @Cacheable(
      value = "tech.yooo.ratel.wind.service.impl.DemoProductServiceImpl",
      key = "caches[0].name+'('+#id+')'")
  @Override
  public DemoProductPoJo redisCache(final long id) {
    return getById(id);
  }

  @Override
  @CacheEvict(
      value = "tech.yooo.ratel.wind.service.impl.DemoProductServiceImpl",
      key = "caches[0].name+'('+#poJo.id+')'")
  @Transactional(rollbackFor = Exception.class)
  public void redisCacheClear(final DemoProductPoJo poJo) {
    update(poJo);
  }

  @Override
  public void deleteNoResult() {}

  @Override
  public int deleteWithResult() {
    return 0;
  }

  @Bean("demoProductBs")
  public BaseService<DemoProductPoJo> setBaseService(@Autowired DemoProductRepository repository) {
    return new BaseServiceImpl<>(repository);
  }

  @SuppressWarnings({"unchecked"})
  @Override
  public <U> U getRepository() throws CommonException {
    return (U) repository;
  }
}
