package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class ConditionGroup<T extends AbstractEntityPoJo<T, ?>> {
  private Condition<T> condition;

  //
  // public AbstractCondition(final QueryColumn<T> queryColumn) {
  //   setEntityInfo(queryColumn.getEntityInfo());
  //   setQueryEntityMetaData(queryColumn.getQueryEntityMetaData());
  // }
  //
  // public AbstractCondition(final Class<T> clazz, String tableName, String tableAlia) {
  //   if (clazz == null && tableName == null && tableAlia == null) {
  //     throw CommonException.of("[clazz,tableName,tableAlia]不能同时为空");
  //   }
  //   final WindConfiguration configuration = AppContextInject.getBean(WindConfiguration.class);
  //   if (clazz != null) {
  //     final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
  //     // 如果tableName不为空,需要覆盖entityInfo的值.传入的tableName优先级最高,因为支持使用不相关的类查询表
  //     if (tableName != null) {
  //       entityInfo.setName(tableName);
  //     } else {
  //       tableName = entityInfo.getName();
  //     }
  //     setEntityInfo(entityInfo);
  //   }
  //   final QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();
  //   queryEntityMetaData.setClazz(clazz);
  //   queryEntityMetaData.setTableName(tableName);
  //   tableAlia = tableAlia == null ? tableName : tableAlia;
  //   queryEntityMetaData.setTableAlia(tableAlia);
  //   String fromTable = tableName;
  //   if (tableAlia != null && !tableAlia.equals(tableName)) {
  //     fromTable = tableName.concat(" ").concat(tableAlia);
  //   }
  //   queryEntityMetaData.setFromTable(fromTable);
  //   setQueryEntityMetaData(queryEntityMetaData);
  // }

  /*
    ConditionGroup 的获取:
    1. 使用Consumer
    2. 传入Condition构造
    TODO-WARN 把LambdaCondition中的方法拷贝过来,实现直接用condition调用原方法
      LambdaContion添加方法or(ConditionGroup)/and(ConditionGroup),实现:获取ConditionGroup的condition,外加括号
      io.github.ramerf.wind.core.condition.LambdaCondition.or(boolean,
      io.github.ramerf.wind.core.condition.Condition<T>)方法签名改了就可以啦
  */
}
