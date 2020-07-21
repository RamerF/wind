package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import lombok.*;
import org.springframework.beans.factory.annotation.Value;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
@Getter
public abstract class AbstractQueryEntity<T extends AbstractEntity> implements QueryEntity<T> {
  protected QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();

  @Setter
  @Value("${wind.enable-logic-delete:true}")
  protected boolean enableLogicDelete = true;

  @Setter
  @Value("${wind.logic-delete-field:isDelete}")
  protected String logicDeleteField = "isDelete";

  @Setter
  @Value("${wind.logic-not-delete:false}")
  protected Object logicNotDelete = false;

  @Setter
  @Value("${wind.logic-deleted:true}")
  protected Object logicDeleted = true;
}
