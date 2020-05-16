package io.github.ramerf.wind.core.repository;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import org.springframework.stereotype.Repository;

/**
 * @author Tang Xiaofeng
 * @since 2020/4/20
 */
@Repository
public interface AbstractBaseRepository extends BaseRepository<AbstractEntity, Long> {}
