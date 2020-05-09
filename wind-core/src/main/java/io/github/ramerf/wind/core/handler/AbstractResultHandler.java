package io.github.ramerf.wind.core.handler;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Method;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * The type Abstract result handler.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /4/6
 */
abstract class AbstractResultHandler<T, E> implements ResultHandler<T, E> {
  /** The Methods. */
  Method[] methods;

  /** The Clazz. */
  final Class<E> clazz;
  /** columnAlia:fieldName} */
  //  final Map<String, String> queryAlias;

  private static final Map<Class<?>, WeakReference<Method[]>> METHODS_MAP = new HashMap<>();

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   * @param queryColumns the query columns
   */
  public AbstractResultHandler(@Nonnull final Class<E> clazz) {
    this(clazz, true);
  }

  /**
   * 初始化数据.
   *
   * @param clazz 支持转换的对象
   * @param queryColumns 查询对象
   * @param initMethods 是否调用初始化methods
   */
  public AbstractResultHandler(@Nonnull final Class<E> clazz, final boolean initMethods) {
    this.clazz = clazz;
    if (initMethods) {
      this.methods =
          Optional.ofNullable(METHODS_MAP.get(clazz))
              .map(Reference::get)
              .orElseGet(
                  () -> {
                    final Method[] methods = clazz.getMethods();
                    METHODS_MAP.put(clazz, new WeakReference<>(methods));
                    return methods;
                  });
    }
    //    this.queryAlias =
    //        queryColumns.stream()
    //            .flatMap(o -> o.getQueryEntityMetaData().getQueryAlias().stream())
    //            .collect(toMap(QueryAlia::getFieldName, QueryAlia::getColumnAlia));
  }

  /**
   * {@inheritDoc}
   *
   * @param t the t
   * @return E the clazz
   */
  @Override
  public abstract E handle(T t);

  /**
   * {@inheritDoc}
   *
   * @param ts the ts
   * @return List the list of clazz
   */
  @Override
  public abstract List<E> handle(List<T> ts);
}
