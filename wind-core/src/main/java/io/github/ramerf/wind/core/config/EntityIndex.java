package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableIndex;
import io.github.ramerf.wind.core.annotation.TableIndex.Index;
import io.github.ramerf.wind.core.annotation.TableIndex.IndexField;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.exception.SimpleException;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * 表索引信息.
 *
 * @author ramer
 * @since 07/12/2020
 */
@Data
public class EntityIndex {
  private String tableName;
  /** 名称. */
  private String name;
  /** 是否唯一. */
  private boolean unique;
  /** 备注. */
  private String comment;

  private IndexColumn[] indexColumns;

  @AllArgsConstructor(staticName = "of")
  private static class IndexColumn {
    /** 索引列. */
    private String column;
    /** 索引长度. */
    private int length;
    /** 索引排序. */
    private Direction direction;
  }

  public static List<EntityIndex> getEntityIndexes(
      @Nonnull final Class<?> clazz,
      final String tableName,
      Set<EntityColumn> entityColumns,
      Dialect dialect) {
    final TableIndex annotation = clazz.getAnnotation(TableIndex.class);
    if (annotation == null) {
      return Collections.emptyList();
    }
    final Index[] indexes = annotation.value();
    List<EntityIndex> entityIndexes = new ArrayList<>();
    for (Index index : indexes) {
      if ("".equals(index.name())) {
        throw new SimpleException("invalid table index: name must be present");
      }
      IndexField[] indexFields = index.indexFields();
      if (indexFields.length == 0) {
        throw new SimpleException("invalid table index: no fields set");
      }
      IndexColumn[] indexColumns = new IndexColumn[indexFields.length];
      for (int i = 0; i < indexFields.length; i++) {
        IndexField indexField = indexFields[i];
        final String field = indexField.field();
        Optional<EntityColumn> optional =
            entityColumns.stream().filter(o -> o.getField().getName().equals(field)).findAny();
        if (!optional.isPresent()) {
          throw new SimpleException(
              "No such field found:{" + field + "}, in " + clazz.getTypeName());
        }
        indexColumns[i] =
            IndexColumn.of(optional.get().getName(), indexField.length(), indexField.direction());
      }
      final EntityIndex entityIndex = new EntityIndex();
      entityIndex.tableName = tableName;
      entityIndex.name = index.name();
      entityIndex.unique = index.unique();
      entityIndex.comment = index.comment();
      entityIndex.indexColumns = indexColumns;
      entityIndexes.add(entityIndex);
    }
    return entityIndexes;
  }

  public String getSqlDefinition(final Dialect dialect) {
    String colums =
        Arrays.stream(indexColumns)
            .map(
                indexColumn ->
                    indexColumn.column
                        + (indexColumn.length == -1 ? " " : "(" + indexColumn.length + ") ")
                        + indexColumn.direction.name())
            .collect(Collectors.joining(","));
    return String.format(
        "create %sindex %s on %s(%s)", unique ? "unique " : "", name, tableName, colums);
  }
}
