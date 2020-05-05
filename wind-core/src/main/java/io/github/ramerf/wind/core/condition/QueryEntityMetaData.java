package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.*;

import static io.github.ramerf.wind.core.util.BeanUtils.methodToProperty;

/**
 * The type Query entity meta data.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /12/28
 */
public class QueryEntityMetaData<T extends AbstractEntity> extends EntityMetaData<T> {
  /** 查询字段. */
  protected List<QueryAlia> queryAlias = new ArrayList<>();

  /** 对应sql语句from后的table字符串. */
  @Getter(AccessLevel.PROTECTED)
  @Setter
  protected String fromTable;

  /** 如果没有设置查询列,这里默认返回所有列,作为扩展,之后可以用注解定义. @return the query alias */
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
          .map(o -> QueryAlia.of(methodToProperty(o.getName()), getTableAlia(), getTableName()))
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
