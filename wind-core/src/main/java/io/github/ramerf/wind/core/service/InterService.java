package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Getter;

import static java.util.stream.Collectors.toList;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2020 /1/5
 */
public interface InterService<T, ID extends Serializable> {

  /**
   * 过滤某些属性可能包含的特殊字符.
   *
   * @param trans 原始对象
   * @param filtered 过滤后的对象
   */
  default void textFilter(T trans, T filtered) {}

  /**
   * Gets query column.
   *
   * @return the query column
   */
  default QueryColumn<T> getQueryColumn() {
    return QueryColumn.fromClass(getPoJoClass());
  }

  /**
   * Gets query.
   *
   * @return the query
   */
  default Query<T> getQuery() {
    return Query.getInstance(getPoJoClass());
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  default Update<T> getUpdate() {
    return Update.getInstance(getPoJoClass());
  }

  /**
   * Gets update for clazz.
   *
   * @param clazz 是否当前类的更新组件
   * @return the update
   */
  default <R> Update<R> getUpdate(final Class<R> clazz) {
    return Update.getInstance(clazz);
  }

  /**
   * 获取service泛型PoJo.
   *
   * @return the po jo class
   */
  default Class<T> getPoJoClass() {
    return EntityUtils.getPoJoClass(this);
  }

  /**
   * 该方法作为扩展,可在service接口中获取到repository.
   *
   * @param <U> the type parameter
   * @return the repository
   * @throws RuntimeException the runtime exception
   */
  default <U> U getRepository() throws RuntimeException {
    throw new RuntimeException("Not implemented");
  }

  /** 可用于指定一个操作包含/不包含的字段. */
  class Fields<T> {
    @Getter private final Set<Field> includes = new HashSet<>();
    @Getter private final Set<Field> excludes = new HashSet<>();

    private Fields() {}

    public static <T> Fields<T> of() {
      return new Fields<>();
    }

    public static <T> Fields<T> of(Class<T> clazz) {
      return new Fields<>();
    }

    @SafeVarargs
    public final Fields<T> include(final IFunction<T, ?>... includeFields) {
      this.includes.addAll(
          Arrays.stream(includeFields).map(BeanFunction::getField).collect(toList()));
      return this;
    }

    public final Fields<T> include(final boolean include, final IFunction<T, ?> includeField) {
      if (include) {
        this.includes.add(includeField.getField());
      }
      return this;
    }

    @SafeVarargs
    public final Fields<T> exclude(@Nonnull final IFunction<T, ?>... excludeFields) {
      for (final IFunction<T, ?> function : excludeFields) {
        this.includes.remove(function.getField());
      }
      this.excludes.addAll(
          Arrays.stream(excludeFields).map(BeanFunction::getField).collect(toList()));
      return this;
    }

    public final Fields<T> exclude(
        final boolean exclude, @Nonnull final IFunction<T, ?> excludeField) {
      if (exclude) {
        this.includes.remove(excludeField.getField());
        this.excludes.add(excludeField.getField());
      }
      return this;
    }

    public Set<Field> getIncludeFields() {
      return includes.isEmpty() ? Collections.emptySet() : includes;
    }

    public Set<Field> getExcludeFields() {
      return excludes.isEmpty() ? Collections.emptySet() : excludes;
    }
  }
}
