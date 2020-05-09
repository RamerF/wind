package io.github.ramerf.wind.core.support;

import java.util.Collection;
import java.util.stream.Stream;

/**
 * 支持链式调用的List.
 *
 * @param <E> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/14
 */
@SuppressWarnings("unused")
public interface ChainList<E> {
  /**
   * Stream stream.
   *
   * @return the stream
   */
  Stream<E> stream();

  /**
   * Add chain linked list.
   *
   * @param e the e
   * @return the chain linked list
   */
  ChainLinkedList<E> add(E e);

  /**
   * Remove chain linked list.
   *
   * @param e the e
   * @return the chain linked list
   */
  ChainLinkedList<E> remove(E e);

  /**
   * Remove chain linked list.
   *
   * @param index the index
   * @return the chain linked list
   */
  ChainLinkedList<E> remove(int index);

  /**
   * Set chain linked list.
   *
   * @param index the index
   * @param element the element
   * @return the chain linked list
   */
  ChainLinkedList<E> set(int index, E element);

  /**
   * Add all chain linked list.
   *
   * @param c the c
   * @return the chain linked list
   */
  ChainLinkedList<E> addAll(Collection<? extends E> c);

  /**
   * Remove all chain linked list.
   *
   * @param c the c
   * @return the chain linked list
   */
  ChainLinkedList<E> removeAll(Collection<? extends E> c);

  /**
   * Clear chain linked list.
   *
   * @return the chain linked list
   */
  ChainLinkedList<E> clear();
}
