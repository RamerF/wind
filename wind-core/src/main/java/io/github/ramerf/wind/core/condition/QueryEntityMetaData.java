package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import java.util.ArrayList;
import java.util.List;
import lombok.*;

/**
 * The type Query entity meta data.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /12/28
 */
public class QueryEntityMetaData<T extends AbstractEntity> extends EntityMetaData<T> {
  /** 查询字段. */
  @Getter protected List<QueryAlia> queryAlias = new ArrayList<>();

  /** 对应sql语句from后的table字符串. */
  @Getter @Setter protected String fromTable;

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
