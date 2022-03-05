package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.exception.ReflectiveInvokeException;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.JdbcUtils;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.util.EntityUtils.fieldToColumn;
import static io.github.ramerf.wind.core.util.EntityUtils.getAllColumnFields;

/**
 * The type Bean result handler.
 *
 * @since 2019 /12/27
 * @author ramer
 * @param <E> the type parameter
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<E> {
  private final Class<E> clazz;

  /** 列对应的字段. */
  private static final Map<ClazzColumn, Field> CLAZZ_COLUMN_FIELD =
      Collections.synchronizedMap(new WeakHashMap<>());

  private final Map<String, Field> columnFieldMap;

  public BeanResultHandler(@Nonnull final Class<E> clazz) {
    this.clazz = clazz;
    if (clazz.isAnnotationPresent(TableInfo.class)) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo((Class<?>) clazz);
      columnFieldMap = entityInfo.getColumnFieldMap();
    } else {
      columnFieldMap = new HashMap<>(20);
      getAllColumnFields(clazz).forEach(field -> columnFieldMap.put(fieldToColumn(field), field));
    }
  }

  @Override
  public E handle(final ResultSet rs) throws SQLException {
    final E obj = BeanUtils.initial(clazz);
    ResultSetMetaData rsmd = rs.getMetaData();
    int columnCount = rsmd.getColumnCount();
    for (int index = 1; index <= columnCount; index++) {
      final String column = JdbcUtils.getColumnName(rsmd, index);
      final ClazzColumn clazzColumn = ClazzColumn.of(clazz, column);
      final Field field =
          Optional.ofNullable(CLAZZ_COLUMN_FIELD.get(clazzColumn))
              .orElseGet(
                  () -> {
                    final Field f = columnFieldMap.get(column);
                    CLAZZ_COLUMN_FIELD.put(clazzColumn, f);
                    return f;
                  });
      if (field == null) {
        if (log.isDebugEnabled()) {
          log.warn("No field found for column: " + column);
        }
        continue;
      }
      Object value = rs.getObject(index);
      if (value == null) {
        continue;
      }
      // 如果是数据库数组类型,获取对应的java数组
      if (value instanceof Array) {
        try {
          value = ((Array) value).getArray();
        } catch (SQLException e) {
          log.warn("handle:fail to get array[{}]", e.getMessage());
          log.error(e.getMessage(), e);
        }
      }
      final Object finalValue =
          TypeHandlerHelper.toJavaValue(
              ValueType.of(value, field.getGenericType(), field),
              BeanUtils.getFieldValue(obj, field),
              field);
      try {
        BeanUtils.setFieldValue(obj, field, finalValue);
      } catch (ReflectiveInvokeException e) {
        log.warn(
            "handle:跳过类型不匹配的字段[fieldName:{},paramType:{},valueType:{}]",
            field.getName(),
            field.getType().getSimpleName(),
            Optional.ofNullable(finalValue)
                .map(Object::getClass)
                .map(Class::getSimpleName)
                .orElse(null));
      }
    }
    return obj;
  }

  /**
   * @param map the map
   * @param obj the obj
   * @param field the field
   * @param paramType 关联对象类型
   */
  private void initMappingObj(
      final Map<String, Object> map,
      final Object obj,
      final Field field,
      final Class<?> paramType) {
    final MappingInfo mappingInfo = EntityMapping.get(obj.getClass(), field).orElse(null);
    if (mappingInfo == null) {
      return;
    }
    final Object mappingObj = BeanUtils.initial(paramType);
    final Field referenceField = mappingInfo.getTargetField();
    referenceField.setAccessible(true);
    BeanUtils.setFieldValueIgnoreException(
        mappingObj, referenceField, map.get(mappingInfo.getJoinColumn()));
    BeanUtils.setFieldValueIgnoreException(obj, field, mappingObj);
  }

  @EqualsAndHashCode
  @AllArgsConstructor(staticName = "of")
  private static class ClazzColumn {
    private final Class<?> clazz;
    private final String column;

    @Override
    public String toString() {
      return column;
    }
  }
}
