package io.github.ramerf.wind.core.util;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.InterService;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import javax.persistence.Entity;
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
  /** {@link BaseService} 泛型{@link AbstractEntityPoJo} */
  private static final Map<Class<?>, WeakReference<Class<?>>> SERVICE_POJO_MAP =
      new ConcurrentHashMap<>();

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
   * 获取对象所有非空(<code>private && !static && !transient</code>)属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @see Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName()
   * @return string
   */
  public static <T> List<String> getNonNullColumns(@Nonnull final T t) {
    final List<String> columns =
        getNonNullColumnFields(t).stream().map(EntityUtils::fieldToColumn).collect(toList());
    log.debug("getNonNullColumns:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象所有非空(<code>private && !static && !transient</code>)属性对应的数据库列名,以逗号分割.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @see Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName()
   * @see #getNonNullColumns(Object)
   * @return string
   */
  public static <T> String getNonNullColumn(@Nonnull final T t) {
    final String nonNullColumn = String.join(",", getNonNullColumns(t));
    log.debug("getNonNullColumn:[{}]", nonNullColumn);
    return nonNullColumn;
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
   * 表名: @Entity > @TableName > 类名(驼封转下划线)
   *
   * @param clazz the clazz
   * @param <T> the type t
   * @return the table name
   */
  public static <T> String getTableName(final Class<T> clazz) {
    final Entity annotEntity = clazz.getAnnotation(Entity.class);
    final TableName annotTableName = clazz.getAnnotation(TableName.class);
    // 表名: @Entity > @TableName > 类名(驼封转下划线)
    if (Objects.nonNull(annotEntity)) {
      return annotEntity.name();
    } else if (Objects.nonNull(annotTableName)) {
      return annotTableName.value();
    } else {
      return StringUtils.camelToUnderline(clazz.getSimpleName());
    }
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
            Optional.ofNullable(SERVICE_POJO_MAP.get(serviceClazz))
                .map(Reference::get)
                .orElse(null);
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
    SERVICE_POJO_MAP.put(serviceClazz, new WeakReference<>(classes));
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
