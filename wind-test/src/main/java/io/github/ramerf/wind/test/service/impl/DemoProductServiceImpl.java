package io.github.ramerf.wind.test.service.impl;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.wind.core.condition.Condition.MatchPattern;
import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.repository.BaseRepository;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.test.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.test.entity.response.DemoProductResponse;
import io.github.ramerf.wind.test.repository.DemoProductRepository;
import io.github.ramerf.wind.test.service.DemoProductService;
import java.util.Collections;
import java.util.List;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Slf4j
@Service
public class DemoProductServiceImpl implements DemoProductService {
  @Resource private DemoProductRepository repository;

  /** 注意: 与下方Bean中的名称对应,保证模块内唯一 */
  @Resource(name = "demoProductBS")
  private BaseService<DemoProductPoJo, DemoProductResponse> baseService;

  @Bean("demoProductBS")
  public BaseService<DemoProductPoJo, DemoProductResponse> setBaseService(
      @Autowired DemoProductRepository repository) {
    return new BaseServiceImpl<>(repository);
  }

  @Override
  public Page<DemoProductPoJo> page(
      int page, int size, List<ExtraProp> extraProps, SortColumn... sortColumns) {
    final Page<DemoProductPoJo> poJoPage = baseService.page(page, size, extraProps, sortColumns);
    log.info("page:这里可以执行额外操作[{}]", poJoPage);
    return poJoPage;
  }

  @Override
  public boolean isDuplicate(final String name) {
    return !CollectionUtils.isEmpty(
        list(
            Collections.singletonList(
                ExtraProp.of(DemoProductPoJo::getName, MatchPattern.EQUAL, name))));
  }

  @Override
  public DemoProductPoJo enableCache(final long id) {
    return getById(id);
  }

  @Cacheable(
      value = "tech.yooo.ratel.demomybatis.service.impl.DemoProductServiceImpl",
      key = "caches[0].name+'('+#id+')'")
  @Override
  public DemoProductPoJo redisCache(final long id) {
    return getById(id);
  }

  @Override
  @CacheEvict(
      value = "tech.yooo.ratel.demomybatis.service.impl.DemoProductServiceImpl",
      key = "caches[0].name+'('+#poJo.id+')'")
  @Transactional(rollbackFor = Exception.class)
  public DemoProductPoJo redisCacheClear(final DemoProductPoJo poJo) {
    return update(poJo);
  }

  @Override
  public void deleteNoResult() {}

  @Override
  public int deleteWithResult() {
    return 0;
  }

  @SuppressWarnings({"unchecked"})
  @Override
  public <U extends BaseRepository<DemoProductPoJo, Long>> U getRepository()
      throws CommonException {
    return (U) repository;
  }
}
