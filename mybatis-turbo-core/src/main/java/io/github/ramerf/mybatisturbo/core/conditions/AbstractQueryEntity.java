package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
@Getter
public abstract class AbstractQueryEntity<T extends AbstractEntity> implements QueryEntity<T> {
  protected QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();

  @Value("${mybatis-plus.global-config.db-config.service-delete-field:isDelete}")
  protected String logicDeleteField = "isDelete";

  @Value("${mybatis-plus.global-config.db-config.service-not-delete-value:false}")
  protected Object logicNotDelete = false;

  @Value("${mybatis-plus.global-config.db-config.service-delete-value:true}")
  protected Object logicDeleted = true;

  //  public void setFromTables(final String fromTables) {
  //    this.fromTables = String.join(SEMICOLON, fromTables);
  //  }

  //  public String getFromTables() {
  //    if (StringUtils.isEmpty(fromTables)) {
  //      fromTables =
  //          Optional.ofNullable(clazz.getAnnotation(TableName.class))
  //              .map(TableName::value)
  //              .orElse(StringUtils.camelToUnderline(clazz.getSimpleName()));
  //    }
  //    return CommonException.requireNonNull(
  //        fromTables, "No table name defined.invoke [newInstance] or [setFromTables] method
  // before.");
  //  }
}
