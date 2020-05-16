package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
@Getter
public abstract class AbstractQueryEntity<T extends AbstractEntity> implements QueryEntity<T> {
  protected QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();

  @Value("${wind.logic-delete-field}")
  protected String logicDeleteField = "isDelete";

  @Value("${wind.logic-not-delete:false}")
  protected Object logicNotDelete = false;

  @Value("${wind.logic-deleted:true}")
  protected Object logicDeleted = true;
}
