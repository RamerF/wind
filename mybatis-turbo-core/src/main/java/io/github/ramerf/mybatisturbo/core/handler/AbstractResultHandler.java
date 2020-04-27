package io.github.ramerf.mybatisturbo.core.handler;

import io.github.ramerf.mybatisturbo.core.conditions.QueryColumn;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import javax.annotation.Nonnull;

import static java.util.stream.Collectors.toMap;

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
  final Method[] methods;

  /** The Clazz. */
  final Class<E> clazz;
  /** columnAlia:fieldName} */
  final Map<String, String> queryAlias;

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   * @param queryColumns the query columns
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz, @Nonnull final List<QueryColumn<?>> queryColumns) {
    this(clazz, queryColumns, true);
  }

  /**
   * 初始化数据.
   *
   * @param clazz 支持转换的对象
   * @param queryColumns 查询对象
   * @param initMethods 是否调用初始化methods
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz,
      @Nonnull final List<QueryColumn<?>> queryColumns,
      final boolean initMethods) {
    this.clazz = clazz;
    this.methods = initMethods ? clazz.getMethods() : new Method[0];
    this.queryAlias =
        queryColumns.stream()
            .flatMap(o -> o.getQueryEntityMetaData().getQueryAlias().stream())
            .collect(toMap(QueryAlia::getFieldName, QueryAlia::getColumnAlia));
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
