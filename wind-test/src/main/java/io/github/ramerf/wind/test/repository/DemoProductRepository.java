package io.github.ramerf.wind.test.repository;

import io.github.ramerf.wind.core.repository.BaseRepository;
import io.github.ramerf.wind.test.entity.pojo.DemoProductPoJo;
import org.springframework.stereotype.Repository;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Repository
// @CacheNamespace(implementation = RedisCache.class, eviction = RedisCache.class)
public interface DemoProductRepository extends BaseRepository<DemoProductPoJo, Long> {}
