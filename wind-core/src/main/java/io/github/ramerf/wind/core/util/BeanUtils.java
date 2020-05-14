package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.condition.QueryEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.service.InterService;
import java.beans.FeatureDescriptor;
import java.io.IOException;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.reflection.ReflectionException;
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

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toList;
import static org.springframework.util.StringUtils.tokenizeToStringArray;

/**
 * The type Bean utils.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@SuppressWarnings({"unused"})
public final class BeanUtils {
  private static final Map<Class<?>, WeakReference<Class<?>>> SERVICE_GENERIC = new HashMap<>();
  private static final Map<Class<?>, WeakReference<List<Field>>> POJO_FIELD_MAP = new HashMap<>();

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
                    methodToProperty(f.getName()),
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
  public static List<String> getNullProp(@Nonnull final Object obj) {
    final BeanWrapperImpl wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .filter(o -> Objects.isNull(wrapper.getPropertyValue(o.getName())))
        .map(FeatureDescriptor::getName)
        .collect(toList());
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
        .collect(toList());
  }

  /**
   * 获取对象的属性名.
   *
   * @param obj the obj
   * @return the all prop
   */
  public static List<String> getAllProp(@Nonnull final Object obj) {
    return Stream.of(new BeanWrapperImpl(obj).getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(o -> !Objects.equals("class", o))
        .collect(toList());
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param obj the obj
   * @return the non null prop
   */
  public static List<Field> getAllPrivateFields(@Nonnull final Class<?> obj) {
    final List<Field> allFields = new ArrayList<>();
    retrievePrivateField(obj, allFields);
    final List<Field> fields =
        allFields.stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .collect(toList());
    log.debug("getAllPrivateField:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有值不为null的(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @return the non null prop
   */
  public static <T> List<Field> getNonNullPrivateFields(@Nonnull final T t) {
    final List<Field> allFields = new ArrayList<>();
    retrievePrivateField(t.getClass(), allFields);
    final List<Field> fields =
        allFields.stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.nonNull(invoke(t, field, null)))
            .collect(toList());
    log.debug("getNonNullPrivateFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有值为null的(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @return the non null prop
   */
  public static <T> List<Field> getNullPrivateFields(@Nonnull final T t) {
    final List<Field> allFields = new ArrayList<>();
    retrievePrivateField(t.getClass(), allFields);
    final List<Field> fields =
        allFields.stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.isNull(invoke(t, field, ex -> -1)))
            .collect(toList());
    log.debug("getNullPrivateFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @return the non null prop
   */
  public static List<String> getAllColumn(@Nonnull final List<Field> fields) {
    final List<String> columns = fields.stream().map(BeanUtils::fieldToColumn).collect(toList());
    log.debug("getAllColumn:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @return the non null prop
   */
  public static List<String> getAllColumn(@Nonnull final Class<?> obj) {
    final List<String> columns =
        getAllPrivateFields(obj).stream().map(BeanUtils::fieldToColumn).collect(toList());
    log.debug("getAllColumn:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @return the non null prop
   */
  public static String fieldToColumn(@Nonnull final Field field) {
    String column = null;
    if (field.isAnnotationPresent(Column.class)) {
      column = field.getAnnotation(Column.class).name();
    }
    if (StringUtils.isEmpty(column)) {
      column = camelToUnderline(field.getName());
    }
    return column;
  }

  /** 获取所有(包含父类)private属性. */
  private static void retrievePrivateField(
      @Nonnull Class<?> clazz, @Nonnull final List<Field> fields) {
    do {
      fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
    } while (Objects.nonNull(clazz = clazz.getSuperclass()));
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
  @SuppressWarnings("all")
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
   * 获取Service泛型参数poJo.
   *
   * @param <T> the type parameter
   * @param <S> the type parameter
   * @param service the service
   * @return the poJo class
   */
  @SuppressWarnings("unchecked")
  public static <T extends AbstractEntityPoJo, S extends InterService<T>> Class<T> getPoJoClass(
      S service) {
    return getParamTypeClass((Class<S>) getProxyTarget(service).getClass());
  }

  @SuppressWarnings("unchecked")
  private static <T extends AbstractEntityPoJo, S extends InterService<T>>
      Class<T> getParamTypeClass(Class<S> serviceClazz) {
    Class<T> classes =
        (Class<T>)
            Optional.ofNullable(SERVICE_GENERIC.get(serviceClazz)).map(Reference::get).orElse(null);
    if (Objects.nonNull(classes)) {
      return classes;
    }

    final Type baseServiceType = serviceClazz.getInterfaces()[0].getGenericInterfaces()[0];
    ParameterizedType parameterizedType = (ParameterizedType) baseServiceType;
    final Type[] arguments = parameterizedType.getActualTypeArguments();
    try {
      classes = (Class<T>) Class.forName(arguments[0].getTypeName());
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
    return camelToUnderline(methodToProperty(method));
  }

  /**
   * 通过方法名获取bean属性名.
   *
   * @param name the name
   * @return the string
   */
  @SuppressWarnings("all")
  public static String methodToProperty(String name) {
    if (name.startsWith("is")) {
      name = name.substring(2);
    } else if (name.startsWith("get") || name.startsWith("set")) {
      name = name.substring(3);
    } else {
      throw new ReflectionException(
          "Error parsing property name '" + name + "'.  Didn't start with 'is', 'get' or 'set'.");
    }

    if (name.length() == 1 || (name.length() > 1 && !Character.isUpperCase(name.charAt(1)))) {
      name = name.substring(0, 1).toLowerCase(Locale.ENGLISH) + name.substring(1);
    }

    return name;
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
   * 通过lambda方法引用获取列名.
   *
   * @param <T> the type parameter
   * @param function the function
   * @return the string
   */
  public static <T> String methodToColumn(final IConsumer<T, ?> function) {
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
  @SuppressWarnings("unchecked")
  public static <T> Set<Class<? extends T>> scanClasses(
      String packagePatterns, Class<T> assignableType) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
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
          Class<T> clazz = (Class<T>) Resources.classForName(classMetadata.getClassName());
          if (assignableType == null || assignableType.isAssignableFrom(clazz)) {
            classes.add(clazz);
          }
        } catch (Throwable e) {
          log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", resource, e.toString());
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
   * @return the optional
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

  /**
   * 对于 {@link Field#get(Object)}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.invoke(obj, field, e -> throw e);
   * </pre>
   *
   * @param obj the obj
   * @param field the field
   * @param consumer 异常时的处理,默认返回null
   * @return the optional
   */
  public static Object invoke(Object obj, Field field, Function<Exception, Object> consumer) {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      return field.get(obj);
    } catch (Exception e) {
      return Optional.ofNullable(consumer).map(ex -> ex.apply(e)).orElse(null);
    }
  }

  /**
   * Is primitive type boolean.
   *
   * @param clazz the clazz
   * @return the boolean
   */
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
        .ifPresent(e -> log.info("main:调用失败处理[{}]", e.getClass()));

    log.info("main:[{}]", getAllPrivateFields(Ts.class));

    Stream.of(new BeanWrapperImpl(new Ts()).getPropertyDescriptors())
        .forEach(
            o -> {
              final Method method = o.getReadMethod();
              try {
                final Field f = Method.class.getDeclaredField("signature");
                f.setAccessible(true);
                final Object signature = f.get(method);
                log.info("main:[{}]", signature);
              } catch (Exception e) {
                e.printStackTrace();
              }
            });
  }
}

@SuppressWarnings("unused")
class Ts extends AbstractEntityPoJo {
  @Column(name = "alia")
  private String name;

  private Integer size;
  private transient String nameSize;
}
