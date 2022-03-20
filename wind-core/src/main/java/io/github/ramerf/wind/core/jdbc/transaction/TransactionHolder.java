package io.github.ramerf.wind.core.jdbc.transaction;

import java.util.ArrayDeque;
import java.util.Deque;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

// TODO WARN Executor从这里实时获取事务,即可实现动态数据源
@Slf4j
public class TransactionHolder {
  private static final ThreadLocal<Deque<Transaction>> TRANSACTION_HOLDER =
      ThreadLocal.withInitial(ArrayDeque::new);

  private TransactionHolder() {}

  public static Transaction peek() {
    return TRANSACTION_HOLDER.get().peek();
  }

  public static Transaction push(@Nonnull Transaction transaction) {
    TRANSACTION_HOLDER.get().push(transaction);
    return transaction;
  }

  public static void poll() {
    Deque<Transaction> deque = TRANSACTION_HOLDER.get();
    deque.poll();
    if (deque.isEmpty()) {
      TRANSACTION_HOLDER.remove();
    }
  }

  public static void clear() {
    TRANSACTION_HOLDER.remove();
  }
}
