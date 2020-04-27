package io.github.ramerf.mybatisturbo.core.support;

import java.util.Collection;
import java.util.stream.Stream;

/**
 * 支持链式调用的List.
 *
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
public interface ChainList<E> {
  Stream<E> stream();

  ChainLinkedList<E> add(E e);

  ChainLinkedList<E> remove(E e);

  ChainLinkedList<E> addAll(Collection<? extends E> c);

  ChainLinkedList<E> removeAll(Collection<? extends E> c);

  ChainLinkedList<E> clear();
}
