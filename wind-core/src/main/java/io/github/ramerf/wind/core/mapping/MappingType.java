package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.persistence.*;
import org.springframework.cglib.proxy.MethodProxy;

import static io.github.ramerf.wind.core.factory.QueryColumnFactory.fromClass;

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
 * }*
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
        final AbstractEntityPoJo poJo,
        final Object relationValue,
        final Method method,
        final Field field,
        MethodProxy proxy,
        Object[] args)
        throws Throwable {
      if (poJo.getId() == null) {
        return (T) proxy.invokeSuper(poJo, args);
      }
      final Optional<MappingInfo> optional = EntityMapping.get(poJo.getClass(), field);
      if (!optional.isPresent()) {
        return (T) proxy.invokeSuper(poJo, args);
      }

      final MappingInfo mappingInfo = optional.get();
      final Object value =
          mappingInfo.getField().getName().equals("id")
              ? poJo.getId()
              : BeanUtils.getValue(poJo, mappingInfo.getField(), null);
      final Class<AbstractEntityPoJo> clazz = (Class<AbstractEntityPoJo>) method.getReturnType();
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(fromClass(clazz))
              .stringWhere(condition -> condition.eq(mappingInfo, value))
              .fetchOne(clazz);
      return (T) mapping;
    }
  },
  /** The One to many. */
  ONE_TO_MANY {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo poJo,
        final Object relationValue,
        final Method method,
        final Field field,
        MethodProxy proxy,
        Object[] args)
        throws Throwable {
      if (poJo.getId() == null) {
        return (T) proxy.invokeSuper(poJo, args);
      }
      final Optional<MappingInfo> optional = EntityMapping.get(poJo.getClass(), field);
      if (!optional.isPresent()) {
        return (T) proxy.invokeSuper(poJo, args);
      }
      final MappingInfo mappingInfo = optional.get();

      final Query query = Query.getInstance();
      final Class<?> type = method.getReturnType();
      @SuppressWarnings("unchecked")
      final Object mapping =
          query
              .select(fromClass((Class<AbstractEntityPoJo>) type))
              .stringWhere(condition -> condition.eq(field, poJo.getId()))
              .fetchAll(type);
      return (T) mapping;
    }
  },
  /** The Many to one. */
  MANY_TO_ONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj,
        final Object relationValue,
        final Method method,
        final Field field,
        MethodProxy proxy,
        Object[] args)
        throws Throwable {
      return (T) proxy.invokeSuper(obj, args);
    }
  },
  /** The Many to many. */
  MANY_TO_MANY {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj,
        final Object relationValue,
        final Method method,
        final Field field,
        MethodProxy proxy,
        Object[] args)
        throws Throwable {
      return (T) proxy.invokeSuper(obj, args);
    }
  },
  /** The None. */
  NONE {
    @Override
    public <T> T fetchMapping(
        final AbstractEntityPoJo obj,
        final Object relationValue,
        final Method method,
        final Field field,
        MethodProxy proxy,
        Object[] args)
        throws Throwable {
      return (T) proxy.invokeSuper(obj, args);
    }
  };

  /**
   * Fetch mapping t.
   *
   * @param <T> the type parameter
   * @param obj the obj
   * @param relationValue the relation value
   * @param method the method
   * @param field the field
   * @param proxy the proxy
   * @param args the args
   * @return the t
   * @throws Throwable the throwable
   */
  public abstract <T> T fetchMapping(
      AbstractEntityPoJo obj,
      final Object relationValue,
      final Method method,
      final Field field,
      MethodProxy proxy,
      Object[] args)
      throws Throwable;

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
