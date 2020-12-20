package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
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
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
public interface InterService<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable> {

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
  default <R extends AbstractEntityPoJo<R, ?>> Update<R> getUpdate(final Class<R> clazz) {
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
    throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
  }

  /** 可用于指定一个操作包含/不包含的字段. */
  class Fields<T> {
    @Getter private final List<IFunction<T, ?>> includes = new ArrayList<>();
    @Getter private final List<IFunction<T, ?>> excludes = new ArrayList<>();

    public static <T extends AbstractEntityPoJo<T, ?>> Fields<T> with(Class<T> clazz) {
      return new Fields<>();
    }

    @SafeVarargs
    public final Fields<T> include(final IFunction<T, ?>... includeFields) {
      this.includes.addAll(Arrays.asList(includeFields));
      return this;
    }

    @SafeVarargs
    public final Fields<T> exclude(@Nonnull final IFunction<T, ?>... excludeFields) {
      for (final IFunction<T, ?> function : excludeFields) {
        this.includes.remove(function);
      }
      this.excludes.addAll(Arrays.asList(excludeFields));
      return this;
    }

    public List<Field> getIncludeFields() {
      return includes.isEmpty()
          ? Collections.emptyList()
          : includes.stream().map(BeanFunction::getField).collect(toList());
    }

    public List<Field> getExcludeFields() {
      return excludes.isEmpty()
          ? Collections.emptyList()
          : excludes.stream().map(BeanFunction::getField).collect(toList());
    }
  }
}
