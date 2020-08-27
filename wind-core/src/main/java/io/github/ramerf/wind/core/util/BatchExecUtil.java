package io.github.ramerf.wind.core.util;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

/**
 * 批量执行工具.
 *
 * @since 2020.08.25
 * @author Tang Xiaofeng
 */
@Slf4j
public class BatchExecUtil {

  /**
   * 批量执行,每次执行150条.
   *
   * @param list the list
   * @param consumer the consumer
   * @see #batchExec(List, int, Consumer)
   */
  public static <T> void batchExec(final List<T> list, Consumer<List<T>> consumer) {
    batchExec(list, 150, consumer);
  }

  /**
   * 批量执行,每次操作<code>batchSize</code>条.
   *
   * @param list the list
   * @param batchSize 每次执行的大小
   * @param consumer the consumer
   */
  public static <T> void batchExec(final List<T> list, int batchSize, Consumer<List<T>> consumer) {
    if (CollectionUtils.isEmpty(list)) {
      log.info("batchExec:[empty!]");
      return;
    }
    List<T> subList = new ArrayList<>(batchSize);
    final int total = list.size();
    final int count = total % batchSize == 0 ? total / batchSize : total / batchSize + 1;
    for (int i = 0; i < count; i++) {
      final boolean lastTrunk = i + 1 == count;
      final int start = i * batchSize;
      final int end = Math.min((i + 1) * batchSize, total);
      subList.addAll(list.subList(start, end));
      if (i + 1 == count || subList.size() == batchSize) {
        log.info("batchExec:[start:{},end:{}]", start, end);
        consumer.accept(subList);
        subList.clear();
      }
    }
  }
}
