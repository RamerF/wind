package io.github.ramerf.mybatisturbo.core.repository;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import org.apache.ibatis.annotations.*;
import org.springframework.data.domain.Pageable;

/**
 * 通用dao层.
 *
 * @param <T> the type parameter
 * @param <ID> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /11/13
 */
@SuppressWarnings("unused")
public interface BaseRepository<T extends AbstractEntity, ID extends Serializable>
    extends BaseMapper<T> {

  /**
   * Insert select long.
   *
   * @param param1 the param 1
   * @param param2 the param 2
   * @return the long
   */
  @Update("INSERT INTO ${param1} VALUES ${param2}")
  long insertSelect(@Param("param1") final String param1, @Param("param2") final String param2);

  /**
   * Count long.
   *
   * @param param the param
   * @return the long
   */
  @Select("SELECT COUNT(*) FROM ${param}")
  long count(@Param("param") final String param);

  /**
   * Find one map.
   *
   * @param param1 the param 1
   * @param param2 the param 2
   * @return the map
   */
  @Select("SELECT ${param1} FROM ${param2}")
  Map<String, Object> findOne(
      @Param("param1") final String param1, @Param("param2") final String param2);

  /**
   * Find all list.
   *
   * @param param1 the param 1
   * @param param2 the param 2
   * @return the list
   */
  @Select("SELECT ${param1} FROM ${param2}")
  List<Map<String, Object>> findAll(
      @Param("param1") final String param1, @Param("param2") final String param2);

  /**
   * 分页查询数据.
   *
   * @param param1 查询部分
   * @param param2 表名部分
   * @param page 分页部分
   * @return the list
   */
  @Select("SELECT ${param1} FROM ${param2} LIMIT #{page.pageSize} OFFSET #{page.offset}")
  List<Map<String, Object>> findAllPage(
      @Param("param1") final String param1,
      @Param("param2") final String param2,
      @Param("page") final Pageable page);

  /**
   * 条件删除.
   *
   * @param param1 表名
   * @param param2 删除赋值(is_delete=false)
   * @param param3 条件
   * @return 删除条数
   */
  @Update("UPDATE ${param1} SET ${param2} WHERE ${param3}")
  long deleteBatch(
      @Param("param1") final String param1,
      @Param("param2") final String param2,
      @Param("param3") final String param3);
}
