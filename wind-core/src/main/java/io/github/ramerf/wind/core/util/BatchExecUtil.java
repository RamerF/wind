package io.github.ramerf.wind.core.util;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;

/**
 * 批量执行工具.
 *
 * @since 2020.08.25
 * @author ramer
 */
@Slf4j
public class BatchExecUtil {

  /**
   * 批量执行,每次执行150条.
   *
   * @param list the list
   * @param consumer the consumer
   * @see #batchExec(String, List, int, Consumer)
   */
  public static <T> void batchExec(
      final String taskName, final List<T> list, Consumer<List<T>> consumer) {
    batchExec(taskName, list, 150, consumer);
  }

  /**
   * 批量执行,每次操作<code>batchSize</code>条.
   *
   * @param <T> the type parameter
   * @param taskName 任务名称,用于打印信息
   * @param list the list
   * @param batchSize 每次执行的大小
   * @param consumer the consumer
   */
  public static <T> void batchExec(
      final String taskName, final List<T> list, int batchSize, Consumer<List<T>> consumer) {
    if (CollectionUtils.isEmpty(list)) {
      log.info("batchExec:{}[empty!]", taskName);
      return;
    }
    List<T> subList = new ArrayList<>(batchSize);
    final int total = list.size();
    final int count = total % batchSize == 0 ? total / batchSize : total / batchSize + 1;
    for (int i = 0; i < count; i++) {
      final boolean lastTrunk = i + 1 == count;
      final int start = i * batchSize;
      final int end = lastTrunk ? total : (i + 1) * batchSize;
      subList.addAll(list.subList(start, end));
      if (lastTrunk || subList.size() == batchSize) {
        log.debug("batchExec:{}[start:{},end:{}]", taskName, start, end);
        consumer.accept(subList);
        subList.clear();
      }
    }
  }
}
