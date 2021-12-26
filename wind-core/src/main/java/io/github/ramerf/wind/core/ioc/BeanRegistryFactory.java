package io.github.ramerf.wind.core.ioc;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * @author ramer
 * @since 23/12/2021
 */
public class BeanRegistryFactory {

  public static void registry(final String path, ApplicationContext applicationContext) {
    Map<Class<?>, List<BeanDefinition>> typeBeansMap = applicationContext.TYPE_BEANS_MAP;
    Map<String, BeanDefinition> nameBeanMap = applicationContext.NAME_BEAN_MAP;
    Set<Class<?>> classes;
    try {
      classes = BeanUtils.scanClassesWithAnnotation(path, Bean.class);
    } catch (IOException e) {
      throw new CommonException("Cannot search for class underneath directory " + path);
    }
    for (Class<?> clazz : classes) {
      // 包含Bean注解的类,调用默认构造器初始化
      {
        Bean annotation = clazz.getAnnotation(Bean.class);
        BeanDefinition beanDefinition =
            annotation.singleton() ? new SingletonBeanDefinition() : new PrototypeBeanDefinition();
        beanDefinition.setPrimary(annotation.primary());
        final String name = annotation.name().equals("") ? annotation.value() : annotation.name();
        beanDefinition.setName(getBeanName(clazz, name));
        beanDefinition.setType(clazz);
        beanDefinition.setInstance(BeanUtils.initial(clazz));
        nameBeanMap.put(beanDefinition.getName(), beanDefinition);
        List<BeanDefinition> beanDefinitions = typeBeansMap.get(clazz);
        if (beanDefinitions == null) {
          beanDefinitions = new ArrayList<>();
          beanDefinitions.add(beanDefinition);
          typeBeansMap.put(beanDefinition.getType(), beanDefinitions);
        } else beanDefinitions.add(beanDefinition);
      }

      // 包含Bean注解的方法,不初始化
      Set<Method> methodsWithAnnotation;
      try {
        methodsWithAnnotation = BeanUtils.scanMethodsWithAnnotation(clazz, Bean.class);
      } catch (IOException e) {
        throw new CommonException(
            "Cannot search for method with annotation Bean for class " + clazz);
      }
      for (Method method : methodsWithAnnotation) {
        Bean annotation = method.getAnnotation(Bean.class);
        BeanDefinition beanDefinition =
            annotation.singleton() ? new SingletonBeanDefinition() : new PrototypeBeanDefinition();
        beanDefinition.setPrimary(annotation.primary());
        final String name = annotation.name().equals("") ? annotation.value() : annotation.name();
        beanDefinition.setName(getBeanName(method, name));
        beanDefinition.setType(method.getReturnType());
        nameBeanMap.put(beanDefinition.getName(), beanDefinition);
        beanDefinition.setDefinitionMethod(method);
        List<BeanDefinition> beanDefinitions = typeBeansMap.get(method.getReturnType());
        if (beanDefinitions == null) {
          beanDefinitions = new ArrayList<>();
          beanDefinitions.add(beanDefinition);
          typeBeansMap.put(beanDefinition.getType(), beanDefinitions);
        } else beanDefinitions.add(beanDefinition);
      }
    }
    // TODO WARN 根据order排序后执行,通过方法定义Bean的初始化
    List<BeanDefinition> beanDefinitions = new LinkedList<>(nameBeanMap.values());
    beanDefinitions.sort(Comparator.comparing(BeanDefinition::getOrder));
    for (int i = 0; i < beanDefinitions.size(); i++) {
      BeanDefinition beanDefinition = beanDefinitions.get(i);
      Method definitionMethod = beanDefinition.getDefinitionMethod();
      if (definitionMethod == null) {
        continue;
      }
      // 持有当前方法的对象
      Object ownObj = applicationContext.getBean(definitionMethod.getDeclaringClass());
      BeanUtils.invoke(ownObj, definitionMethod);
    }
    // TODO WARN 执行bean依赖字段(@Resource)注入

  }

  private static String getBeanName(final Class<?> clazz, @Nonnull final String inputName) {
    if (!inputName.equals("")) {
      return inputName;
    }
    char[] chars = clazz.getSimpleName().toCharArray();
    if (chars[0] < 97 || chars[0] > 122) {
      chars[0] += 32;
    }
    return String.valueOf(chars);
  }

  private static String getBeanName(final Method method, @Nonnull final String inputName) {
    if (!inputName.equals("")) {
      return inputName;
    }
    char[] chars = method.getName().toCharArray();
    if (chars[0] < 97 || chars[0] > 122) {
      chars[0] += 32;
    }
    return String.valueOf(chars);
  }
}
