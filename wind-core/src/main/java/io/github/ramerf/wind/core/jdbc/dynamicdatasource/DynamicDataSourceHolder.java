/*
 * Copyright © 2018 organization baomidou
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.ramerf.wind.core.jdbc.dynamicdatasource;

import io.github.ramerf.wind.core.util.StringUtils;
import java.util.ArrayDeque;
import java.util.Deque;
import lombok.extern.slf4j.Slf4j;

/**
 * Copy from {@link com.baomidou.dynamic.datasource.toolkit.DynamicDataSourceContextHolder}
 *
 * <p>基于ThreadLocal的切换数据源工具类
 *
 * @since 2022.03.20
 * @author ramer
 */
@Slf4j
public final class DynamicDataSourceHolder {
  private static final ThreadLocal<Deque<String>> LOOKUP_KEY_HOLDER =
      ThreadLocal.withInitial(
          () -> {
            final ArrayDeque<String> arrayDeque = new ArrayDeque<>();
            arrayDeque.push("primary");
            return arrayDeque;
          });

  private DynamicDataSourceHolder() {}

  /** 获取当前线程数据源 */
  public static String peek() {
    final String ds = LOOKUP_KEY_HOLDER.get().peek();
    if (log.isDebugEnabled()) {
      log.debug(Thread.currentThread().getName() + " peek DynamicDataSource: " + ds);
    }
    return ds;
  }

  /** 清空再添加. */
  static String clearPush(String ds) {
    clear();
    return push(ds);
  }

  public static String push(String ds) {
    String key = StringUtils.isEmpty(ds) ? "primary" : ds;
    if (log.isDebugEnabled()) {
      log.debug(Thread.currentThread().getName() + " push DynamicDataSource: " + key);
    }
    LOOKUP_KEY_HOLDER.get().push(key);
    return key;
  }

  /**
   * 获取并移除当前线程数据源
   *
   * <p>如果当前线程是连续切换数据源 只会移除掉当前线程的数据源名称
   */
  public static String poll() {
    Deque<String> deque = LOOKUP_KEY_HOLDER.get();
    if (log.isDebugEnabled()) {
      log.debug(Thread.currentThread().getName() + " poll DynamicDataSource");
    }
    if (deque.isEmpty()) {
      LOOKUP_KEY_HOLDER.remove();
    }
    return deque.poll();
  }

  /**
   * 强制清空本地线程
   *
   * <p>防止内存泄漏，如手动调用了push可调用此方法确保清除
   */
  public static void clear() {
    if (log.isDebugEnabled()) {
      log.debug(Thread.currentThread().getName() + " clear DynamicDataSource.");
    }
    LOOKUP_KEY_HOLDER.remove();
  }
}
