package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.OneToOne;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.StringCondition;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import java.lang.reflect.Field;
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
      final Class<T> fetchClazz = (Class<T>) mappingInfo.getReferenceClazz();
      final QueryColumn<T> queryColumn = QueryColumn.fromClass(fetchClazz);
      final StringCondition<T> stringCondition = StringCondition.getInstance(queryColumn);
      return Query.getInstance(fetchClazz)
          .select(queryColumn)
          .where(stringCondition.eq(mappingInfo, relationValue))
          .fetchOne(fetchClazz);
    }
  },
  /** The One to many. */
  ONE_TO_MANY {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      //
      // final Class<?> referenceClazz = mappingInfo.getReferenceClazz();
      // // 如果是一对多,查询多的一方的关联关系
      // final Optional<MappingInfo> infactOpt = EntityMapping.get(referenceClazz, t.getClass());
      // if (!infactOpt.isPresent()) {
      //   throw CommonException.of(
      //       "No mapping object [" + referenceClazz + "] found in " + t.getClass());
      // }
      // final MappingInfo infactMapping = infactOpt.get();
      //
      // final Class<?> type = mappingInfo.getClazz();
      // final QueryColumn<E> queryColumn = QueryColumn.fromClass((Class<E>) type);
      // final StringCondition<E> condition = StringCondition.getInstance(queryColumn);
      // @SuppressWarnings("unchecked")
      // final Object mapping =
      //     Query.getInstance((Class<E>) poJo.getClass())
      //         .select(queryColumn)
      //         .where(condition.eq(mappingInfo.getField(), relationValue))
      //         .fetchAll(type);
      // return (T) mapping;
      return null;
    }
  },
  /** The Many to one. */
  MANY_TO_ONE {
    @Override
    public <T, E> T fetchMapping(
        final E poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> type = mappingInfo.getReferenceClazz();
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
    if (field.isAnnotationPresent(io.github.ramerf.wind.core.annotation.OneToMany.class)) {
      return ONE_TO_MANY;
    }
    if (field.isAnnotationPresent(io.github.ramerf.wind.core.annotation.ManyToOne.class)) {
      return MANY_TO_ONE;
    }
    // if (field.isAnnotationPresent(ManyToMany.class)) {
    //   throw CommonException.of("方法不支持");
    // return MANY_TO_MANY;
    // }
    return NONE;
  }
}
