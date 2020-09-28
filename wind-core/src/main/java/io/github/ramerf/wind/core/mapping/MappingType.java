package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
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
      return (T) new OneToOneFetch(poJo, mappingInfo, relationValue).getFetchProxy();
    }
  },
  /** The One to many. */
  ONE_TO_MANY {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo poJo, final MappingInfo field, final Object relationValue) {
      return null;
    }
  },
  /** The Many to one. */
  MANY_TO_ONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj, final MappingInfo field, final Object relationValue) {
      return null;
    }
  },
  /** The Many to many. */
  MANY_TO_MANY {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj, final MappingInfo field, final Object relationValue) {
      return null;
    }
  },
  /** The None. */
  NONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj, final MappingInfo field, final Object relationValue) {
      return null;
    }
  };

  /**
   * Fetch mapping t.
   *
   * @param <T> the type parameter
   * @param obj the obj
   * @param field the field
   * @param relationValue
   * @return the t
   */
  public abstract <T> T fetchMapping(
      final AbstractEntityPoJo obj, final MappingInfo field, final Object relationValue);

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
    if (field.isAnnotationPresent(ManyToMany.class)) {
      return MANY_TO_MANY;
    }
    return NONE;
  }
}
