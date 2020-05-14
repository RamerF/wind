package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.InterService;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.framework.AdvisedSupport;
import org.springframework.aop.framework.AopProxy;
import org.springframework.aop.support.AopUtils;

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toList;

/**
 * The type Bean utils.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@SuppressWarnings({"unused"})
public final class EntityUtils {
  private static final Map<Class<?>, WeakReference<Class<?>>> SERVICE_GENERIC = new HashMap<>();
  private static final Map<Class<?>, WeakReference<List<Field>>> POJO_FIELD_MAP = new HashMap<>();

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)保存到数据库的属性.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param obj the obj
   * @return the non null field
   */
  public static List<Field> getAllColumnFields(@Nonnull final Class<?> obj) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(obj, new ArrayList<>()).stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .collect(toList());
    log.debug("getAllColumnFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)保存到数据库且值不为null的属性.<br>
   *
   * @return the non null Field
   */
  public static <T> List<Field> getNonNullColumnFields(@Nonnull final T t) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(t.getClass(), new ArrayList<>()).stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.nonNull(BeanUtils.invoke(t, field, null)))
            .collect(toList());
    log.debug("getNonNullColumnFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)保存到数据库且值为null的属性.<br>
   *
   * @return the non null field
   */
  public static <T> List<Field> getNullColumnFields(@Nonnull final T t) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(t.getClass(), new ArrayList<>()).stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.isNull(BeanUtils.invoke(t, field, ex -> -1)))
            .collect(toList());
    log.debug("getNullColumnFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @see Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName()
   * @return string
   */
  public static List<String> getAllColumns(@Nonnull final Class<?> obj) {
    final List<String> columns =
        getAllColumnFields(obj).stream().map(EntityUtils::fieldToColumn).collect(toList());
    log.debug("getAllColumn:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @see Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName()
   * @return string
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
    Class<S> serviceClazz = (Class<S>) getProxyTarget(service).getClass();
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
    log.info("main:[{}]", getAllColumnFields(Ts.class));
  }
}
