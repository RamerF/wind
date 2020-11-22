package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * 逻辑删除配置.
 *
 * @author Tang Xiaofeng
 * @since 2020/7/26
 */
@Slf4j
@Setter
@Getter
public class LogicDeleteProp {
  /** 是否开启逻辑删除. */
  private boolean enable = false;

  /** 逻辑删除字段. */
  private String fieldName;

  /** 逻辑未删除值. */
  private boolean notDelete = false;

  /** 逻辑已删除值. */
  private boolean deleted = true;

  public static LogicDeleteProp of(@Nonnull final WindConfiguration configuration) {
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(configuration.getLogicDeleteProp().isEnable());
    logicDeleteProp.setFieldName(configuration.getLogicDeleteProp().getFieldName());
    logicDeleteProp.setDeleted(configuration.getLogicDeleteProp().isDeleted());
    logicDeleteProp.setNotDelete(configuration.getLogicDeleteProp().isNotDelete());
    return logicDeleteProp;
  }

  public static LogicDeleteProp of(
      final TableInfo tableInfo, @Nonnull final WindConfiguration configuration) {
    if (tableInfo == null) {
      return of(configuration);
    }
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(tableInfo.logicDelete().enable());
    logicDeleteProp.setFieldName(tableInfo.logicDelete().fieldName());
    logicDeleteProp.setDeleted(tableInfo.logicDelete().deleted());
    logicDeleteProp.setNotDelete(tableInfo.logicDelete().notDelete());
    return logicDeleteProp;
  }

  @SuppressWarnings("rawtypes")
  public <ID extends Serializable> ID getIdClass(final Class<? extends AbstractEntityPoJo> clazz) {
    Type superClass = clazz.getGenericSuperclass();
    for (; ; ) {
      if (!(superClass instanceof ParameterizedType)) {
        continue;
      }
      ParameterizedType parameterizedType = (ParameterizedType) superClass;
      final Type[] arguments = parameterizedType.getActualTypeArguments();
      if (arguments.length == 2 && arguments[0] instanceof AbstractEntityPoJo) {
        @SuppressWarnings("unchecked")
        final ID id = (ID) arguments[1];
        return id;
      }
      if (superClass.equals(AbstractEntityPoJo.class)) {
        return null;
      }
    }
  }
}
