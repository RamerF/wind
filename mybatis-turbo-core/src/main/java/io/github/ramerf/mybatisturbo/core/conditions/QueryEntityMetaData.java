package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.mybatisturbo.core.util.CollectionUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.*;
import org.apache.ibatis.reflection.property.PropertyNamer;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
public class QueryEntityMetaData<T extends AbstractEntity> extends EntityMetaData<T> {
  protected List<QueryAlia> queryAlias = new ArrayList<>();

  @Getter(AccessLevel.PROTECTED)
  @Setter
  protected String fromTable;

  /** 如果没有设置查询列,这里默认返回所有列,作为扩展,之后可以用注解定义. */
  public List<QueryAlia> getQueryAlias() {
    if (CollectionUtils.isEmpty(queryAlias)) {
      return Stream.of(
              CommonException.requireNonNull(
                      getClazz(),
                      "Specify clazz when use wildcard query,see: newInstance(Class<?> clazz)")
                  .getMethods())
          .filter(
              o ->
                  (o.getName().startsWith("set") || o.getName().startsWith("is"))
                      && o.getParameterTypes().length == 1)
          .map(
              o ->
                  QueryAlia.of(
                      PropertyNamer.methodToProperty(o.getName()), getTableAlia(), getTableName()))
          .collect(Collectors.toList());
    }
    return queryAlias;
  }

  /**
   * 获取group by条件.
   *
   * @return the {@link GroupByClause}
   */
  public GroupByClause<T> getGroupByClause() {
    GroupByClause<T> groupByClause = new GroupByClause<>();
    groupByClause.setTableAlia(this.tableAlia);
    return groupByClause;
  }
}
