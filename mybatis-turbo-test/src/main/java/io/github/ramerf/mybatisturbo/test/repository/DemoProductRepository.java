package io.github.ramerf.mybatisturbo.test.repository;

import io.github.ramerf.mybatisturbo.core.config.RedisCache;
import io.github.ramerf.mybatisturbo.core.repository.BaseRepository;
import io.github.ramerf.mybatisturbo.test.entity.pojo.DemoProductPoJo;
import org.apache.ibatis.annotations.CacheNamespace;
import org.springframework.stereotype.Repository;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Repository
@CacheNamespace(implementation = RedisCache.class, eviction = RedisCache.class)
public interface DemoProductRepository extends BaseRepository<DemoProductPoJo, Long> {}
