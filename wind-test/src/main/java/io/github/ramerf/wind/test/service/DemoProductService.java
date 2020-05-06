package io.github.ramerf.wind.test.service;

import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.test.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.test.entity.response.DemoProductResponse;

/**
 * The interface Demo product service.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/17
 */
@SuppressWarnings("unused")
public interface DemoProductService extends BaseService<DemoProductPoJo, DemoProductResponse> {
  /**
   * 判断商品名称是否重复.
   *
   * @param name the name
   * @return true :重复
   */
  boolean isDuplicate(final String name);

  /**
   * Enable cache demo product po jo.
   *
   * @param id the id
   * @return the demo product po jo
   */
  DemoProductPoJo enableCache(final long id);

  /**
   * Redis cache demo product po jo.
   *
   * @param id the id
   * @return the demo product po jo
   */
  DemoProductPoJo redisCache(final long id);

  /**
   * Redis cache clear demo product po jo.
   *
   * @param poJo the po jo
   * @return the demo product po jo
   */
  DemoProductPoJo redisCacheClear(DemoProductPoJo poJo);

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
