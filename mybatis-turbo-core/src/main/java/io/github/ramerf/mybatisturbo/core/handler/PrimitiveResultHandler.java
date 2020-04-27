package io.github.ramerf.mybatisturbo.core.handler;

import io.github.ramerf.mybatisturbo.core.conditions.QueryColumn;
import io.github.ramerf.mybatisturbo.core.util.CollectionUtils;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 基本类型结果转换,注意该类使用valueOf转换值,如果没有该方法将会报错.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/27
 */
@Slf4j
public class PrimitiveResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {

  public PrimitiveResultHandler(
      @Nonnull final Class<E> clazz, @Nonnull final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns, false);
  }

  /**
   * {@inheritDoc}<br>
   *
   * @throws NoSuchMethodException
   */
  @Override
  @SuppressWarnings("unchecked")
  public E handle(Map<String, Object> map) {
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final Object value = map.entrySet().stream().findFirst().map(Entry::getValue).orElse(null);
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (E) String.valueOf(value);
    }
    if (value instanceof Long) {
      return (E) Long.valueOf(value.toString());
    }
    try {
      return (E) value.getClass().getConstructor(value.getClass()).newInstance(value);
    } catch (Exception e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return (E) value;
  }

  @Override
  public List<E> handle(List<Map<String, Object>> maps) {
    if (CollectionUtils.isEmpty(maps)) {
      return Collections.emptyList();
    }
    return maps.stream().map(this::handle).collect(Collectors.toList());
  }
}
