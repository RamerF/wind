package io.github.ramerf.wind.core.support;

import java.util.*;
import java.util.stream.Stream;
import lombok.ToString;

/**
 * 支持链式调用的List.
 *
 * @author ramer
 * @since 2020/1/14
 */
@ToString
public class ChainLinkedList<E> implements ChainList<E> {
  private final List<E> list = new LinkedList<>();

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
  public ChainLinkedList<E> remove(final int index) {
    list.remove(index);
    return this;
  }

  @Override
  public ChainLinkedList<E> set(final int index, final E element) {
    list.set(index, element);
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
