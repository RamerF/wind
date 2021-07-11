package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.QueryColumn.QueryAlia;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * The type Query entity meta data.
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2019 /12/28
 */
public class QueryEntityMetaData<T> extends EntityMetaData<T> {
  /** 查询字段. */
  @Getter protected List<QueryAlia> queryAlias = new ArrayList<>();

  /** 对应sql语句from后的table字符串. */
  @Setter protected String fromTable;

  /** 是否需要包含表别名.默认为false,仅含有子表查询时,设置为true. */
  @Setter private boolean containTableAlia = false;

  public String getFromTable() {
    // containTableAlia为true 或者 别名不等于表名时 需要拼接
    if (containTableAlia || (StringUtils.nonEmpty(tableAlia) && !tableAlia.equals(tableName))) {
      return tableName.concat(" ").concat(tableAlia);
    }
    return tableName;
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
