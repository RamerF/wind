package io.github.ramerf.mybatisturbo.core.support;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
public class Lists {
  public static <E> ChainLinkedList<E> chainLinkedList() {
    return new ChainLinkedList<>();
  }
}
