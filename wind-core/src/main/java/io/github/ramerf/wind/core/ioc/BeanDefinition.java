package io.github.ramerf.wind.core.ioc;

import java.lang.reflect.Method;

/**
 * @author ramer
 * @since 2021.12.24
 */
public interface BeanDefinition {
  void setType(final Class<?> type);

  Class<?> getType();

  void setName(final String name);

  String getName();

  void setPrimary(final boolean primary);
  /** 首要的,当相同类型的bean存在多个时,必须指定且只能指定一个首要的bean */
  boolean isPrimary();

  void setSingleton(final boolean singleton);
  /** 单例bean */
  boolean isSingleton();

  void setOrder(final int order);
  /** bean初始化,后初始化顺序. */
  int getOrder();

  void setDefinitionMethod(final Method method);

  Method getDefinitionMethod();

  void setInstance(final Object instance);

  /** 获取bean */
  <T> T get();
}
