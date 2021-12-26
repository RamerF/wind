package io.github.ramerf.wind.core.ioc;

import java.lang.reflect.Method;

/**
 * @author ramer
 * @since 23/12/2021
 */
public abstract class AbstractBeanDefinition implements BeanDefinition {
  private Class<?> clazz;
  private String name;
  /** 首要的,当相同类型的bean存在多个时,必须指定且只能指定一个首要的bean */
  private boolean primary;

  /** 单例bean */
  private boolean singleton;

  /** bean初始化,后初始化顺序. */
  private int order;

  /** 定义bean的方法,该值不为空时表示由方法定义的bean */
  private Method definitionMethod;

  /** 是否通过方法定义的bean */
  private boolean methodBean;

  /** 注入类型 */
  //  private InjectType injectType;
  /** 获取bean */
  private Object instance;

  @Override
  public void setType(Class<?> type) {
    this.clazz = type;
  }

  @Override
  public Class<?> getType() {
    return clazz;
  }

  @Override
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public void setPrimary(boolean primary) {
    this.primary = primary;
  }

  @Override
  public boolean isPrimary() {
    return primary;
  }

  @Override
  public void setSingleton(boolean singleton) {
    this.singleton = singleton;
  }

  @Override
  public boolean isSingleton() {
    return singleton;
  }

  @Override
  public void setOrder(int order) {
    this.order = order;
  }

  @Override
  public int getOrder() {
    return order;
  }

  @Override
  public void setDefinitionMethod(final Method definitionMethod) {
    this.definitionMethod = definitionMethod;
  }

  @Override
  public Method getDefinitionMethod() {
    return definitionMethod;
  }

  @Override
  public void setInstance(final Object instance) {
    this.instance = instance;
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T> T get() {
    return (T) instance;
  }
}
