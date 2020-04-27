package io.github.ramerf.mybatisturbo.core.util;

import io.github.ramerf.mybatisturbo.core.conditions.IFunction;
import io.github.ramerf.mybatisturbo.core.conditions.QueryEntity;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.constant.Constant;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.service.InterService;
import java.beans.FeatureDescriptor;
import java.io.IOException;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.reflection.property.PropertyNamer;
import org.springframework.aop.framework.AdvisedSupport;
import org.springframework.aop.framework.AopProxy;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.ClassMetadata;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.util.ClassUtils;

import static org.springframework.util.StringUtils.tokenizeToStringArray;

/**
 * The type Bean utils.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@SuppressWarnings({"unused", "DeprecatedIsStillUsed"})
public final class BeanUtils {
  private static final Map<Class<?>, WeakReference<Class<?>[]>> SERVICE_GENERIC = new HashMap<>();

  /**
   * Map转Bean.
   *
   * @param <R> the type parameter
   * @param map the map
   * @param clazz the clazz
   * @return the r
   */
  public static <R> R mapToBean(Map<String, Object> map, Class<R> clazz) {
    final R r = initial(clazz);
    Stream.of(clazz.getMethods())
        .filter(o -> o.getName().startsWith("set") && o.getParameterTypes().length == 1)
        .forEach(
            f -> {
              final Object value = map.get(methodToColumn(f.getName()));
              try {
                f.setAccessible(true);
                f.invoke(r, value);
              } catch (Exception e) {
                log.info(
                    "mapToBean:跳过类型不匹配的字段[{} {}->{}]",
                    PropertyNamer.methodToProperty(f.getName()),
                    f.getParameterTypes()[0].getSimpleName(),
                    value.getClass().getSimpleName());
              }
            });
    return r;
  }

  /**
   * 获取对象属性值为空的属性名.
   *
   * @param obj the obj
   * @return the null prop
   */
  public static List<String> getNullProp(Object obj) {
    final BeanWrapperImpl wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .filter(o -> Objects.isNull(wrapper.getPropertyValue(o.getName())))
        .map(FeatureDescriptor::getName)
        .collect(Collectors.toList());
  }

  /**
   * 获取对象属性值不为空的属性名.
   *
   * @param obj the obj
   * @return the non null prop
   */
  public static List<String> getNonNullProp(Object obj) {
    final BeanWrapper wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .filter(o -> Objects.nonNull(wrapper.getPropertyValue(o.getName())))
        .map(FeatureDescriptor::getName)
        .filter(o -> !Objects.equals("class", o))
        .collect(Collectors.toList());
  }

  /**
   * 获取对象的属性名.
   *
   * @param obj the obj
   * @return the all prop
   */
  public static List<String> getAllProp(Object obj) {
    return Stream.of(new BeanWrapperImpl(obj).getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(o -> !Objects.equals("class", o))
        .collect(Collectors.toList());
  }

  /**
   * 实例化对象 .
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the t
   */
  public static <T> T initial(final Class<T> clazz) {
    try {
      return clazz.newInstance();
    } catch (InstantiationException | IllegalAccessException e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw CommonException.of(String.format("无法实例化对象[%s]", clazz.getSimpleName()));
  }

  /**
   * 实例化对象.
   *
   * @param <T> the type parameter
   * @param classPath the class path
   * @return the t
   */
  @SuppressWarnings("unchecked")
  public static <T> T initial(final String classPath) {
    try {
      return (T) initial(getClazz(classPath));
    } catch (Exception e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw CommonException.of(String.format("无法实例化对象[%s]", classPath));
  }

  /**
   * 通过类路径获取Class.
   *
   * @param <T> the type parameter
   * @param classPath the class path
   * @return the clazz
   */
  @SuppressWarnings({"unchecked", "AlibabaUndefineMagicConstant"})
  public static <T> Class<T> getClazz(String classPath) {
    if (classPath.contains("/")) {
      classPath = classPath.replaceAll("/", ".");
    }
    try {
      return (Class<T>) Class.forName(classPath);
    } catch (Exception e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw CommonException.of(String.format("无法获取class[%s]", classPath));
  }

  /**
   * TODO-WARN 获取类的私有变量.
   *
   * @param clazz the clazz
   * @param includeParent 包含父类变量
   * @return the private fields
   */
  public static List<String> getPrivateFields(Class<?> clazz, final boolean includeParent) {
    List<String> fields = new ArrayList<>();
    retrieveFields(clazz, includeParent, new StringBuilder(), fields);
    return fields;
  }

  /**
   * Retrieve fields list.
   *
   * @param clazz the clazz
   * @param includeParent the include parent
   * @param prefix the prefix
   * @param fields the fields
   */
  @SuppressWarnings("ConstantConditions")
  private static void retrieveFields(
      final Class<?> clazz,
      final boolean includeParent,
      final StringBuilder prefix,
      List<String> fields) {
    if (includeParent && !Objects.equals(clazz.getSuperclass(), Object.class)) {
      retrieveFields(clazz.getSuperclass(), includeParent, prefix, fields);
    }
    Stream.of(clazz.getDeclaredFields())
        .filter(
            field ->
                Modifier.isPrivate(field.getModifiers())
                    && !Modifier.isStatic(field.getModifiers()))
        .forEach(
            field -> {
              if (Modifier.isPrivate(field.getModifiers())
                  && !Modifier.isStatic(field.getModifiers())) {
                final String name = field.getName();
                if (AbstractEntity.class.isAssignableFrom(field.getType())) {
                  prefix.append(name).append(Constant.DEFAULT_STRING_POINT);
                  retrieveFields(field.getType(), true, prefix, fields);
                } else {
                  fields.add(prefix.toString().concat(field.getName()));
                }
              }
            });
  }

  /**
   * 如果在service方法之外获取泛型参数poJo,需要调用这个方法,因为service被代理了.
   *
   * @param <T> the type parameter
   * @param <E> the type parameter
   * @param <S> the type parameter
   * @param service the service
   * @return the poJo class
   */
  @SuppressWarnings("unchecked")
  public static <
          T extends AbstractEntityPoJo, E extends AbstractEntity, S extends InterService<T, E>>
      Class<T> getPoJoClass(S service) {
    return (Class<T>) getParamTypeClass((Class<S>) getProxyTarget(service).getClass())[0];
  }

  /**
   * Gets po jo class. <br>
   * 替换为{@link BeanUtils#getPoJoClass(InterService)}
   *
   * @param <T> the type parameter
   * @param <E> the type parameter
   * @param <S> the type parameter
   * @param serviceClazz the service clazz
   * @return the po jo class
   */
  @SuppressWarnings("unchecked")
  @Deprecated
  public static <
          T extends AbstractEntityPoJo, E extends AbstractEntity, S extends InterService<T, E>>
      Class<T> getPoJoClass(Class<S> serviceClazz) {
    return (Class<T>) getParamTypeClass(serviceClazz)[0];
  }

  /**
   * Gets response class. 参考getPoJoClass方法.
   *
   * @param <T> the type parameter
   * @param <E> the type parameter
   * @param <S> the type parameter
   * @param serviceClazz the service clazz
   * @return the response class
   */
  @SuppressWarnings("unchecked")
  @Deprecated
  public static <
          T extends AbstractEntityPoJo, E extends AbstractEntity, S extends InterService<T, E>>
      Class<E> getResponseClass(Class<S> serviceClazz) {
    return (Class<E>) getParamTypeClass(serviceClazz)[1];
  }

  private static <
          T extends AbstractEntityPoJo, E extends AbstractEntity, S extends InterService<T, E>>
      Class<?>[] getParamTypeClass(Class<S> serviceClazz) {
    Class<?>[] classes =
        Optional.ofNullable(SERVICE_GENERIC.get(serviceClazz))
            .map(Reference::get)
            .orElse(new Class[2]);
    if (Objects.nonNull(classes[0])) {
      return classes;
    }

    final Type baseServiceType = serviceClazz.getInterfaces()[0].getGenericInterfaces()[0];
    ParameterizedType parameterizedType = (ParameterizedType) baseServiceType;
    final Type[] arguments = parameterizedType.getActualTypeArguments();
    try {
      classes[0] = Class.forName(arguments[0].getTypeName());
      classes[1] = Class.forName(arguments[1].getTypeName());
    } catch (ClassNotFoundException ignored) {
      throw CommonException.of("无法获取父类泛型");
    }
    SERVICE_GENERIC.put(serviceClazz, new WeakReference<>(classes));
    return classes;
  }

  /**
   * 通过方法名获取列名.
   *
   * @param method the method
   * @return the string
   */
  public static String methodToColumn(final String method) {
    if (Objects.isNull(method) || method.length() < 1) {
      return method;
    }
    return StringUtils.camelToUnderline(PropertyNamer.methodToProperty(method));
  }

  /**
   * 通过lambda方法引用获取列名.
   *
   * @param <T> the type parameter
   * @param function the function
   * @return the string
   */
  public static <T> String methodToColumn(final IFunction<T, ?> function) {
    return methodToColumn(LambdaUtils.getMethodName(function));
  }

  /**
   * 获取指定包下,指定接口/类的子类.
   *
   * @param packagePatterns 支持多个包名分隔符: ",; \t\n"
   * @param assignableType 父接口/类
   * @return 所有子类 set
   * @throws IOException the IOException
   */
  public static Set<Class<?>> scanClasses(String packagePatterns, Class<?> assignableType)
      throws IOException {
    Set<Class<?>> classes = new HashSet<>();
    String[] packagePatternArray =
        tokenizeToStringArray(
            packagePatterns, ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS);
    for (String packagePattern : packagePatternArray) {
      Resource[] resources =
          new PathMatchingResourcePatternResolver()
              .getResources(
                  ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX
                      .concat(ClassUtils.convertClassNameToResourcePath(packagePattern))
                      .concat("/**/*.class"));
      for (Resource resource : resources) {
        try {
          ClassMetadata classMetadata =
              new CachingMetadataReaderFactory().getMetadataReader(resource).getClassMetadata();
          Class<?> clazz = Resources.classForName(classMetadata.getClassName());
          if (assignableType == null || assignableType.isAssignableFrom(clazz)) {
            classes.add(clazz);
          }
        } catch (Throwable e) {
          log.warn("Cannot load the '" + resource + "'. Cause by " + e.toString());
        }
      }
    }
    return classes;
  }

  /**
   * 对于 {@link Method#invoke(Object, Object...)}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.invoke(null, String.class.getMethods()[0], "string")
   *         .ifPresent(e -> log.info(" BeanUtils.main:调用失败处理[{}]", e.getClass()));
   * </pre>
   *
   * @param obj the obj
   * @param method the method
   * @param value the value
   */
  public static Optional<Exception> invoke(Object obj, Method method, Object... value) {
    try {
      if (!method.isAccessible()) {
        method.setAccessible(true);
      }
      method.invoke(obj, value);
      return Optional.empty();
    } catch (Exception e) {
      return Optional.of(e);
    }
  }

  public static boolean isPrimitiveType(final Class<?> clazz) {
    return (ClassUtils.isPrimitiveOrWrapper(clazz)
        || Enum.class.isAssignableFrom(clazz)
        || CharSequence.class.isAssignableFrom(clazz)
        || Number.class.isAssignableFrom(clazz)
        || Date.class.isAssignableFrom(clazz)
        || URI.class == clazz
        || URL.class == clazz
        || Locale.class == clazz
        || Class.class == clazz);
  }

  /**
   * 获取代理对象的目标对象.
   *
   * @param proxy 代理对象
   */
  private static Object getProxyTarget(Object proxy) {
    if (!AopUtils.isAopProxy(proxy)) {
      return proxy;
    }
    if (AopUtils.isJdkDynamicProxy(proxy)) {
      try {
        return getJdkDynamicProxyTargetObject(proxy);
      } catch (Exception e) {
        log.warn(e.getMessage());
        log.error(e.getMessage(), e);
      }
    } else {
      try {
        return getCglibProxyTargetObject(proxy);
      } catch (Exception e) {
        log.warn(e.getMessage());
        log.error(e.getMessage(), e);
      }
    }
    return proxy;
  }

  private static Object getCglibProxyTargetObject(Object proxy) throws Exception {
    Field field = proxy.getClass().getDeclaredField("CGLIB$CALLBACK_0");
    field.setAccessible(true);
    Object dynamicAdvisedInterceptor = field.get(proxy);
    Field advised = dynamicAdvisedInterceptor.getClass().getDeclaredField("advised");
    advised.setAccessible(true);
    return ((AdvisedSupport) advised.get(dynamicAdvisedInterceptor)).getTargetSource().getTarget();
  }

  private static Object getJdkDynamicProxyTargetObject(Object proxy) throws Exception {
    Field h = proxy.getClass().getSuperclass().getDeclaredField("h");
    h.setAccessible(true);
    AopProxy aopProxy = (AopProxy) h.get(proxy);
    Field advised = aopProxy.getClass().getDeclaredField("advised");
    advised.setAccessible(true);
    return ((AdvisedSupport) advised.get(aopProxy)).getTargetSource().getTarget();
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   * @throws Exception the exception
   */
  public static void main(String[] args) throws Exception {
    BeanUtils.scanClasses("io.github.ramerf", QueryEntity.class)
        .forEach(o -> log.info("main:[{}]", o));

    invoke(null, String.class.getMethods()[0], "string");

    invoke(null, String.class.getMethods()[0], "string")
        .ifPresent(e -> log.info(" BeanUtils.main:调用失败处理[{}]", e.getClass()));
  }
}
