package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.OneToOne;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import java.lang.reflect.Field;
import javax.annotation.Nonnull;
import javax.persistence.*;

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
 *    @ManyToMany
 *    private List<Foo> foos;
 *  }
 * }
 *
 * @author Tang Xiaofeng
 * @since 2020.09.19
 */
@SuppressWarnings({"unchecked", "DuplicatedCode"})
public enum MappingType {
  /** The One to one. */
  ONE_TO_ONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> type = mappingInfo.getReferenceClazz();
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(QueryColumnFactory.fromClass((Class<AbstractEntityPoJo>) type))
              .stringWhere(condition -> condition.eq(mappingInfo, relationValue))
              .fetchOne(type);
      return (T) mapping;
    }
  },
  /** The One to many. */
  ONE_TO_MANY {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> type = mappingInfo.getClazz();
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(QueryColumnFactory.fromClass((Class<AbstractEntityPoJo>) type))
              .stringWhere(condition -> condition.eq(mappingInfo.getField(), relationValue))
              .fetchAll(type);
      return (T) mapping;
    }
  },
  /** The Many to one. */
  MANY_TO_ONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj, final MappingInfo mappingInfo, final Object relationValue) {
      final Class<?> type = mappingInfo.getReferenceClazz();
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(QueryColumnFactory.fromClass((Class<AbstractEntityPoJo>) type))
              .stringWhere(condition -> condition.eq(mappingInfo, relationValue))
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
    public <T> T fetchMapping(
        final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
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
  public abstract <T> T fetchMapping(
      final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue);

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
    if (field.isAnnotationPresent(ManyToMany.class)) {
      throw CommonException.of("方法不支持");
      // return MANY_TO_MANY;
    }
    return NONE;
  }
}
