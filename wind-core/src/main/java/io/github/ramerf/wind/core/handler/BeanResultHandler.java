package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.lang.reflect.Field;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.Map.Entry;
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
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {

  /** 列对应的字段. */
  private static final Map<ClazzColumn, Field> CLAZZ_COLUMN_FIELD =
      Collections.synchronizedMap(new WeakHashMap<>());

  private final Map<String, Field> columnFieldMap;

  public BeanResultHandler(@Nonnull final Class<E> clazz) {
    super(clazz);
    if (clazz.isAnnotationPresent(TableInfo.class)) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo((Class<?>) clazz);
      columnFieldMap = entityInfo.getColumnFieldMap();
    } else {
      columnFieldMap = new HashMap<>(20);
      getAllColumnFields(clazz).forEach(field -> columnFieldMap.put(fieldToColumn(field), field));
    }
  }

  @Override
  // @SuppressWarnings({"unchecked", "rawtypes"})
  public E handle(Map<String, Object> map) {
    // map = {alia:value}
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final E obj = BeanUtils.initial(clazz);
    for (Entry<String, Object> entry : map.entrySet()) {
      final String column = entry.getKey();
      Object value = entry.getValue();
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
      final ClazzColumn clazzColumn = ClazzColumn.of(clazz, column);
      final Field field =
          Optional.ofNullable(CLAZZ_COLUMN_FIELD.get(clazzColumn))
              .orElseGet(
                  () -> {
                    final Field f = columnFieldMap.get(column);
                    CLAZZ_COLUMN_FIELD.put(clazzColumn, f);
                    return f;
                  });
      if (field != null) {
        final Object finalValue =
            TypeHandlerHelper.toJavaValue(
                ValueType.of(value, field.getGenericType(), field),
                BeanUtils.getValue(obj, field, null),
                field);
        BeanUtils.setValue(
            obj,
            field,
            finalValue,
            exception ->
                log.warn(
                    "handle:跳过类型不匹配的字段[fieldName:{},paramType:{},valueType:{}]",
                    field.getName(),
                    field.getType().getSimpleName(),
                    Optional.ofNullable(finalValue)
                        .map(Object::getClass)
                        .map(Class::getSimpleName)
                        .orElse(null)));
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
    BeanUtils.setValue(mappingObj, referenceField, map.get(mappingInfo.getJoinColumn()), null);
    BeanUtils.setValue(obj, field, mappingObj, null);
  }

  @EqualsAndHashCode
  @AllArgsConstructor(staticName = "of")
  private static class ClazzColumn {
    private final Class<?> clazz;
    private final String column;
  }
}
