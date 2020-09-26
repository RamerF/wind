package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
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
  List<Method> methods;

  /** The Clazz. */
  final Class<E> clazz;
  /** columnAlia:fieldName} */
  Map<String, String> fieldAliaMap = new HashMap<>();

  private static final Map<Class<?>, WeakReference<List<Method>>> METHODS_MAP =
      new ConcurrentHashMap<>();

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    this(clazz, queryColumns, true);
  }

  /**
   * 初始化数据.
   *
   * @param clazz 支持转换的对象
   * @param initMethods 是否调用初始化methods
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz,
      final List<QueryColumn<?>> queryColumns,
      final boolean initMethods) {
    this.clazz = clazz;
    if (initMethods) {
      this.methods =
          Optional.ofNullable(METHODS_MAP.get(clazz))
              .map(Reference::get)
              .orElseGet(
                  () -> {
                    final List<Method> methods = BeanUtils.getWriteMethods(clazz);
                    METHODS_MAP.put(clazz, new WeakReference<>(methods));
                    return methods;
                  });
      // 这里由于查询所有字段使用了*,所以字段名不匹配的时候,无法赋值,为了兼容性,需要把*转换为具体的字段
      if (queryColumns != null) {
        this.fieldAliaMap =
            queryColumns.stream()
                .flatMap(o -> o.getQueryEntityMetaData().getQueryAlias().stream())
                .collect(toMap(QueryAlia::getFieldName, QueryAlia::getColumnAlia));
      }
    }
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
  public List<E> handle(List<T> ts) {
    if (CollectionUtils.isEmpty(ts)) {
      return Collections.emptyList();
    }
    return ts.stream().map(this::handle).collect(Collectors.toList());
  }
}
