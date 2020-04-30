package io.github.ramerf.mybatisturbo.test.service;

import io.github.ramerf.mybatisturbo.core.service.BaseService;
import io.github.ramerf.mybatisturbo.test.entity.pojo.DemoProductPoJo;
import io.github.ramerf.mybatisturbo.test.entity.response.DemoProductResponse;

/**
 * The interface Demo product service.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/17
 */
public interface DemoProductService extends BaseService<DemoProductPoJo, DemoProductResponse> {
  /**
   * 判断商品名称是否重复.
   *
   * @param name the name
   * @return true:重复
   */
  boolean isDuplicate(final String name);

  DemoProductPoJo enableCache(final long id);

  DemoProductPoJo redisCache(final long id);

  DemoProductPoJo redisCacheClear(DemoProductPoJo poJo);

  void deleteNoResult();

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
