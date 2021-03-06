package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.lang.reflect.Method;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Abstract result handler.
 *
 * @since 2020 /4/6
 * @author ramer
 * @param <P> 数据库对应 poJo
 * @param <T> 数据库返回对象
 * @param <E> 实际返回对象
 */
@Slf4j
public abstract class AbstractResultHandler<P, T, E> implements ResultHandler<T, E> {
  /** The Methods. */
  List<Method> methods;

  /** The Clazz. */
  final Class<E> clazz;
  /** columnAlia:fieldName} */
  Map<String, String> fieldAliaMap = new HashMap<>();

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz, final List<QueryColumn<P>> queryColumns) {
    this(clazz, queryColumns, true);
  }

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   */
  @SafeVarargs
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz, @Nonnull final QueryColumn<P>... queryColumns) {
    this(clazz, Arrays.asList(queryColumns), true);
  }

  /**
   * 初始化数据.
   *
   * @param clazz 支持转换的对象
   * @param initMethods 是否调用初始化methods
   */
  public AbstractResultHandler(
      @Nonnull final Class<E> clazz,
      final List<QueryColumn<P>> queryColumns,
      final boolean initMethods) {
    this.clazz = clazz;
    // if (initMethods) {
    //   this.methods =
    //       Optional.ofNullable(METHODS_MAP.get(clazz))
    //           .map(Reference::get)
    //           .orElseGet(
    //               () -> {
    //                 final List<Method> methods = BeanUtils.getWriteMethods(clazz);
    //                 METHODS_MAP.put(clazz, new WeakReference<>(methods));
    //                 return methods;
    //               });
    //   // 这里由于查询所有字段使用了*,所以字段名不匹配的时候,无法赋值,为了兼容性,需要把*转换为具体的字段
    //   if (queryColumns != null) {
    //     this.fieldAliaMap =
    //         queryColumns.stream()
    //             .flatMap(o -> o.getQueryEntityMetaData().getQueryAlias().stream())
    //             .filter(o -> o.getFieldName() != null && o.getColumnAlia() != null)
    //             .collect(toMap(QueryAlia::getFieldName, QueryAlia::getColumnAlia));
    //   } else if (AbstractEntityPoJo.class.isAssignableFrom(clazz)) {
    //     final Class<AbstractEntityPoJo> entityClass = (Class<AbstractEntityPoJo>) clazz;
    //     @SuppressWarnings("unchecked")
    //     final QueryColumn<? extends AbstractEntityPoJo> queryColumn =
    //         QueryColumn.fromClass(entityClass);
    //     this.fieldAliaMap =
    //         queryColumn.getQueryEntityMetaData().getQueryAlias().stream()
    //             .collect(toMap(QueryAlia::getFieldName, QueryAlia::getColumnAlia));
    //   }
    // }
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
    List<E> es = new ArrayList<>();
    for (T t : ts) {
      es.add(handle(t));
    }
    return es;
  }
}
