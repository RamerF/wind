package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableIndexes;
import io.github.ramerf.wind.core.annotation.TableIndexes.Index;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Data;
import org.springframework.data.domain.Sort.Direction;

/**
 * 表索引信息.
 *
 * @author ramer
 * @since 07/12/2020
 */
@Data
public class EntityIndex {
  /** 名称. */
  private String name;
  /** 是否唯一. */
  private boolean unique;
  /** 索引列. */
  private List<String> columns;
  /** 备注. */
  private String comment;
  /** 索引排序. */
  private Direction direction;

  public static List<EntityIndex> getEntityIndexes(@Nonnull final Class<?> clazz, Dialect dialect) {
    TableIndexes annotation = clazz.getAnnotation(TableIndexes.class);
    Index[] indexes = annotation.value();
    List<EntityIndex> entityIndexes = new ArrayList<>();
    for (int i = 0; i < indexes.length; i++) {
      Index index = indexes[i];
      EntityIndex entityIndex = new EntityIndex();
      entityIndex.name = index.name();
      entityIndex.unique = index.unique();
      entityIndex.comment = index.comment();
      entityIndex.direction = index.direction();
      EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
      List<String> columns = new ArrayList<>();
      entityIndex.setColumns(columns);
      List<EntityColumn> entityColumns = entityInfo.getEntityColumns();
      String[] fields = index.fields();
      for (String field : fields) {
        Optional<EntityColumn> optional =
            entityColumns.stream().filter(o -> o.getField().getName().equals(field)).findAny();
        if (!optional.isPresent()) {
          throw SimpleException.of("No such field found in " + clazz.getTypeName());
        }
        columns.add(optional.get().getName());
      }
      entityIndexes.add(entityIndex);
    }
    return entityIndexes;
  }

  private String getSqlDefinition(@Nonnull final Class<?> clazz, final Dialect dialect) {
    List<EntityIndex> indexes = getEntityIndexes(clazz, dialect);
    // TODO-WARN 索引 sql
    return "creat index " + name + " on ";
  }
}
