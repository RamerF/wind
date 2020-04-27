package io.github.ramerf.mybatisturbo.core.support;

import java.util.*;
import java.util.stream.Stream;

/**
 * 支持链式调用的List.
 *
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
public class ChainLinkedList<E> implements ChainList<E> {
  private List<E> list = new LinkedList<>();

  @Override
  public Stream<E> stream() {
    return list.stream();
  }

  @Override
  public ChainLinkedList<E> add(final E e) {
    list.add(e);
    return this;
  }

  @Override
  public ChainLinkedList<E> remove(final E e) {
    list.remove(e);
    return this;
  }

  @Override
  public ChainLinkedList<E> addAll(final Collection<? extends E> c) {
    list.addAll(c);
    return this;
  }

  @Override
  public ChainLinkedList<E> removeAll(final Collection<? extends E> c) {
    list.removeAll(c);
    return this;
  }

  @Override
  public ChainLinkedList<E> clear() {
    list.clear();
    return this;
  }
}
