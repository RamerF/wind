package io.github.ramerf.wind.core.ioc;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 容器上下文.
 *
 * <p>过程:
 * <li>加载指定路径(WindVersion.class对应的根路径)所有的Class
 * <li>注册Bean,并调用默认构造器实例化
 * <li>根据order排序后,执行依赖bean的注入
 *
 * @author ramer
 * @since 2021.12.24
 */
public class ApplicationContext {
  final Map<String, BeanDefinition> NAME_BEAN_MAP = new ConcurrentHashMap<>();
  final Map<Class<?>, List<BeanDefinition>> TYPE_BEANS_MAP = new ConcurrentHashMap<>();

  public ApplicationContext(final String beanPath) {
    // BeanRegistryFactory.registry(beanPath, this);
    // Phase 1. 保存所有class
    // Phase 2. class转为BeanDefinition保存
    // Phase 3. sort + poopulateBean

  }

  /** 通过名称获取bean. */
  public <T> T getBean(final String name) {
    if (!NAME_BEAN_MAP.containsKey(name)) {
      throw new NoSuchBeanException(name);
    }
    BeanDefinition bean = NAME_BEAN_MAP.get(name);
    return bean.get();
  }

  /** 通过类型获取bean. */
  public <T> T getBean(final Class<?> clazz) {
    if (!TYPE_BEANS_MAP.containsKey(clazz)) {
      throw new NoSuchBeanException(clazz);
    }
    List<BeanDefinition> beans = TYPE_BEANS_MAP.get(clazz);
    if (beans.size() > 1) {
      Optional<BeanDefinition> primary =
          beans.stream().filter(BeanDefinition::isPrimary).findFirst();
      if (primary.isPresent()) {
        return primary.get().get();
      }
      throw new NonUniqueBeanException(clazz, beans);
    }
    return beans.get(0).get();
  }
}
