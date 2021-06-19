package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.StringCondition;
import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.*;
import java.util.Objects;
import java.util.Optional;
import javax.annotation.Nonnull;

/**
 * The enum Mapping type.
 *
 * <pre>
 * {@code
 *  public class F {
 *    @OneToOne
 *    private Foo foo;
 *
 *    @OneToMany
 *    private List<Foo> foos;
 *
 *    @ManyToOne
 *    private Foo foo;
 *  }
 * }
 *
 * @author ramer
 * @since 2020.09.19
 */
@SuppressWarnings({"unchecked", "DuplicatedCode"})
public enum MappingType {
  /** The One to one. */
  ONE_TO_ONE {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<T> fetchClazz = (Class<T>) mappingInfo.getTargetClazz();
      final QueryColumn<T> queryColumn = QueryColumn.fromClass(fetchClazz);
      final StringCondition<T> stringCondition = StringCondition.getInstance(queryColumn);
      return Query.getInstance(fetchClazz)
          .select(queryColumn)
          .where(stringCondition.eq(mappingInfo, relationValue))
          .fetchOne(fetchClazz);
    }

    @Override
    MappingInfo populateMappingInfo(final Field field) {
      final MappingInfo mappingInfo = new MappingInfo();
      mappingInfo.setMappingType(this);
      mappingInfo.setClazz(field.getDeclaringClass());
      mappingInfo.setField(field);
      mappingInfo.setColumn(EntityUtils.fieldToColumn(field, true));
      final Class<?> targetClazz = field.getType();
      mappingInfo.setTargetClazz(targetClazz);

      final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
      mappingInfo.setShouldJoinColumn(oneToOne.shouldJoinColumn());
      final String targetFieldStr = oneToOne.targetField();
      final Field targetField;
      final Field idField = EntityHelper.getEntityIdField(targetClazz);
      if (idField == null) {
        throw new IllegalStateException(String.format("no id defined in %s", targetClazz));
      }
      if (!"".equals(targetFieldStr)) {
        targetField = BeanUtils.getDeclaredField(mappingInfo.getTargetClazz(), targetFieldStr);
      } else {
        targetField = idField;
      }
      Objects.requireNonNull(targetField, "target field could not null");
      mappingInfo.setTargetField(targetField);
      if (mappingInfo.isShouldJoinColumn()) {
        return mappingInfo;
      }
      final OneToOne targetOneToOne = targetField.getAnnotation(OneToOne.class);
      if (!targetOneToOne.shouldJoinColumn()) {
        throw new IllegalStateException(
            String.format(
                "%s %s should one and only one field with shouldJoinColumn specified",
                field.getDeclaringClass().getName(), field.getName()));
      }
      String targetJoinColumn = targetOneToOne.joinColumn();
      if (!"".equals(targetJoinColumn)) {
        mappingInfo.setTargetColumn(targetJoinColumn);
      } else if (idField.equals(targetField)) {
        mappingInfo.setTargetColumn(
            EntityUtils.getTableName(targetClazz) + "_" + EntityUtils.fieldToColumn(idField, true));
      } else {
        mappingInfo.setTargetColumn(EntityUtils.fieldToColumn(targetField, true));
      }
      return mappingInfo;
    }
  },
  /** 一对多,多的一方必须关联一的一方. */
  ONE_TO_MANY {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> referenceClazz = mappingInfo.getTargetClazz();
      // 必须关联一的一方
      final Optional<MappingInfo> infactOpt =
          EntityMapping.get(referenceClazz, mappingInfo.getTargetField());
      if (!infactOpt.isPresent()) {
        throw SimpleException.of(
            "No mapping object [" + referenceClazz + "] found in " + poJo.getClass());
      }
      final QueryColumn<E> queryColumn = QueryColumn.fromClass((Class<E>) referenceClazz);
      final StringCondition<E> condition = StringCondition.getInstance(queryColumn);
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance((Class<E>) poJo.getClass())
              .select(queryColumn)
              .where(condition.eq(mappingInfo.getTargetColumn(), relationValue))
              .fetchAll(referenceClazz);
      return (T) mapping;
    }

    @Override
    MappingInfo populateMappingInfo(final Field field) {
      final MappingInfo mappingInfo = new MappingInfo();
      mappingInfo.setMappingType(this);
      mappingInfo.setClazz(field.getDeclaringClass());
      mappingInfo.setField(field);
      mappingInfo.setColumn(EntityUtils.fieldToColumn(field, true));
      final Type type = ((ParameterizedType) field.getGenericType()).getActualTypeArguments()[0];
      final Class<?> targetClazz = (Class<?>) type;
      mappingInfo.setTargetClazz(targetClazz);

      final OneToMany oneToMany = field.getAnnotation(OneToMany.class);
      String joinColumn = oneToMany.joinColumn();
      // 手动指定关联列名
      if (!"".equals(joinColumn)) {
        mappingInfo.setTargetColumn(joinColumn);
      } else {
        mappingInfo.setTargetColumn(
            EntityUtils.getTableName(targetClazz)
                + "_"
                + EntityUtils.fieldToColumn(EntityHelper.getEntityIdField(targetClazz), true));
      }
      return mappingInfo;
    }
  },
  /** The Many to one. */
  MANY_TO_ONE {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> type = mappingInfo.getTargetClazz();
      final QueryColumn<E> queryColumn = QueryColumn.fromClass((Class<E>) type);
      final StringCondition<E> stringCondition = StringCondition.getInstance(queryColumn);
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance((Class<E>) poJo.getClass())
              .select(queryColumn)
              .where(stringCondition.eq(mappingInfo, relationValue))
              .fetchOne(type);
      return (T) mapping;
    }

    @Override
    MappingInfo populateMappingInfo(final Field field) {
      // TODO WARN 从这里开始
      final MappingInfo mappingInfo = new MappingInfo();
      mappingInfo.setMappingType(this);
      mappingInfo.setClazz(field.getDeclaringClass());
      mappingInfo.setField(field);
      mappingInfo.setColumn(EntityUtils.fieldToColumn(field, true));
      final Class<?> targetClazz = field.getType();
      mappingInfo.setTargetClazz(targetClazz);
      mappingInfo.setShouldJoinColumn(true);

      final ManyToOne manyToOne = field.getAnnotation(ManyToOne.class);
      final String targetFieldStr = manyToOne.targetField();
      final Field targetField;
      final Field idField = EntityHelper.getEntityIdField(targetClazz);
      if (idField == null) {
        throw new IllegalStateException(String.format("no id defined in %s", targetClazz));
      }
      if (!"".equals(targetFieldStr)) {
        targetField = BeanUtils.getDeclaredField(mappingInfo.getTargetClazz(), targetFieldStr);
      } else {
        targetField = idField;
      }
      Objects.requireNonNull(targetField, "target field could not null");
      mappingInfo.setTargetField(targetField);
      if (mappingInfo.isShouldJoinColumn()) {
        return mappingInfo;
      }
      final OneToOne targetOneToOne = targetField.getAnnotation(OneToOne.class);
      if (!targetOneToOne.shouldJoinColumn()) {
        throw new IllegalStateException(
            String.format(
                "%s %s should one and only one field with shouldJoinColumn specified",
                field.getDeclaringClass().getName(), field.getName()));
      }
      String targetJoinColumn = targetOneToOne.joinColumn();
      if (!"".equals(targetJoinColumn)) {
        mappingInfo.setTargetColumn(targetJoinColumn);
      } else if (idField.equals(targetField)) {
        mappingInfo.setTargetColumn(
            EntityUtils.getTableName(targetClazz) + "_" + EntityUtils.fieldToColumn(idField, true));
      } else {
        mappingInfo.setTargetColumn(EntityUtils.fieldToColumn(targetField, true));
      }
      return mappingInfo;
    }
  },

  // /** The Many to many. */
  // MANY_TO_MANY {
  //   @Override
  //   public <T> T fetchMapping(
  //       final AbstractEntityPoJo obj, final MappingInfo mappingInfo, final Object relationValue)
  // {
  //     throw CommonException.of("方法不支持");
  //   }
  // },
  /** The None. */
  NONE {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      return null;
    }

    @Override
    MappingInfo populateMappingInfo(final Field field) {
      return null;
    }
  },
  ;

  /**
   * Fetch mapping t.
   *
   * @param <T> the type parameter
   * @param poJo the po jo
   * @param mappingInfo the mapping info
   * @param relationValue the relation value
   * @return the t
   */
  public abstract <T, E> T fetchMapping(
      final E poJo, final MappingInfo mappingInfo, final Object relationValue);

  abstract MappingInfo populateMappingInfo(final Field field);

  public static MappingInfo getMappingInfo(final Field field) {
    return MappingType.of(field).populateMappingInfo(field);
  }

  /**
   * Of mapping type.
   *
   * @param field the field
   * @return the mapping type
   */
  public static @Nonnull MappingType of(final Field field) {
    if (field.isAnnotationPresent(OneToOne.class)) {
      return ONE_TO_ONE;
    }
    if (field.isAnnotationPresent(OneToMany.class)) {
      return ONE_TO_MANY;
    }
    if (field.isAnnotationPresent(ManyToOne.class)) {
      return MANY_TO_ONE;
    }
    /* 后续可能会支持
    if (field.isAnnotationPresent(ManyToMany.class)) {
      throw CommonException.of("方法不支持");
      return MANY_TO_MANY;
    }*/
    return NONE;
  }
}
