package io.github.ramerf.wind.demo.service;

import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.demo.entity.pojo.Foo;

/**
 * The interface Demo product service.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/17
 */
@SuppressWarnings("unused")
public interface FooService extends BaseService<Foo> {

  /**
   * Enable cache demo product po jo.
   *
   * @param id the id
   * @return the demo product po jo
   */
  Foo enableCache(final long id);

  /**
   * Redis cache demo product po jo.
   *
   * @param id the id
   * @return the demo product po jo
   */
  Foo redisCache(final long id);

  /**
   * Redis cache clear demo product po jo.
   *
   * @param poJo the po jo
   */
  void redisCacheClear(Foo poJo);

  /** Delete no result. */
  void deleteNoResult();

  /**
   * Delete with result int.
   *
   * @return the int
   */
  int deleteWithResult();

  /*
  create,
  createBatch,
  getOne,
  list,
  update,
  updateBatch,
  delete,
  deleteBatch,
  */
}
