package io.github.ramerf.mybatisturbo.core.repository;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import org.springframework.stereotype.Repository;

/**
 * @author Tang Xiaofeng
 * @since 2019/11/13
 */
@Repository
public interface AbstractBaseRepository extends BaseRepository<AbstractEntity, Long> {}
