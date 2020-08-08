package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.InterService;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import javax.persistence.Entity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.framework.AdvisedSupport;
import org.springframework.aop.framework.AopProxy;
import org.springframework.aop.support.AopUtils;

import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.*;
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
   * 获取对象所有{@code private && !static && !transient}保存到数据库的属性.<br>
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
    if (log.isTraceEnabled()) {
      log.trace("getAllColumnFields:[{}]", fields);
    }
    return fields;
  }

  /**
   * 获取对象所有{@code private && !static && !transient}保存到数据库且值不为null的属性.<br>
   *
   * @param <T> the type parameter
   * @param t the t
   * @return the non null Field
   */
  public static <T extends AbstractEntity> List<Field> getNonNullColumnFields(@Nonnull final T t) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(t.getClass(), new ArrayList<>()).stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.nonNull(BeanUtils.getValue(t, field, null)))
            .collect(toList());
    if (log.isTraceEnabled()) {
      log.debug("getNonNullColumnFields:[{}]", fields);
    }
    return filterCustomerField(t, fields);
  }

  @SuppressWarnings("unchecked")
  private static <T extends AbstractEntity> List<Field> filterCustomerField(
      @Nonnull final T t, final List<Field> fields) {
    final Class<? extends AbstractEntityPoJo> clazz;
    if (t instanceof AbstractEntityRequest) {
      clazz = ((AbstractEntityRequest<? extends AbstractEntityPoJo>) t).getPoJoClass();
    } else {
      clazz = (Class<? extends AbstractEntityPoJo>) t.getClass();
    }
    final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
    // 剔除掉自定义字段
    Stream<Field> stream = fields.stream();
    // 创建时间
    if (entityInfo.getCreateTimeField() != null) {
      // 可能覆盖父类的字段
      stream =
          stream.filter(
              field ->
                  !CREATE_TIME_FIELD_NAME.equals(field.getName())
                      || field.equals(entityInfo.getCreateTimeField()));
    }
    // 更新时间
    if (entityInfo.getUpdateTimeFiled() != null) {
      stream =
          stream.filter(
              field ->
                  !UPDATE_TIME_FIELD_NAME.equals(field.getName())
                      || field.equals(entityInfo.getUpdateTimeFiled()));
    }
    // 逻辑删除
    final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
    // 未启用 或者 自定义字段
    if (!logicDeleteProp.isEnable()
        || !LOGIC_DELETE_COLUMN_NAME.equals(logicDeleteProp.getColumn())) {
      stream = stream.filter(field -> !LOGIC_DELETE_FIELD_NAME.equals(field.getName()));
    }
    return stream.collect(toList());
  }

  /**
   * 获取对象所有{@code private && !static && !transient}保存到数据库且值为null的属性.<br>
   *
   * @param <T> the type parameter
   * @param t the t
   * @return the non null field
   */
  public static <T> List<Field> getNullColumnFields(@Nonnull final T t) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(t.getClass(), new ArrayList<>()).stream()
            .filter(field -> Modifier.isPrivate(field.getModifiers()))
            .filter(field -> !Modifier.isStatic(field.getModifiers()))
            .filter(field -> !Modifier.isTransient(field.getModifiers()))
            .filter(field -> Objects.isNull(BeanUtils.getValue(t, field, ex -> -1)))
            .collect(toList());
    log.debug("getNullColumnFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象所有{@code private && !static && !transient}属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param obj the obj
   * @return string all columns
   * @see Column#name() Column#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static List<String> getAllColumns(@Nonnull final Class<?> obj) {
    final List<String> columns =
        getAllColumnFields(obj).stream().map(EntityUtils::fieldToColumn).collect(toList());
    log.debug("getAllColumn:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象所有非空{@code private && !static && !transient}属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param <T> the type parameter
   * @param t the t
   * @return string non null columns
   * @see Column#name() Column#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static <T extends AbstractEntity> List<String> getNonNullColumns(@Nonnull final T t) {
    final List<String> columns =
        getNonNullColumnFields(t).stream().map(EntityUtils::fieldToColumn).collect(toList());
    log.debug("getNonNullColumns:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象所有非空{@code private && !static && !transient}属性对应的数据库列名,以逗号分割.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param <T> the type parameter
   * @param t the t
   * @return string non null column
   * @see Column#name() Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   * @see #getNonNullColumns(AbstractEntity)
   */
  public static <T extends AbstractEntity> String getNonNullColumn(@Nonnull final T t) {
    final String nonNullColumn = String.join(",", getNonNullColumns(t));
    log.debug("getNonNullColumn:[{}]", nonNullColumn);
    return nonNullColumn;
  }

  /**
   * 获取对象属性对应的数据库列名.<br>
   * 默认值为{@link Column#name()};如果前者为空,值为<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param field the field
   * @return string string
   * @see Column#name() Column#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
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
   * 表名: {@link TableInfo#name()} &gt; {@link Entity#name()} &gt; 类名(驼封转下划线)
   *
   * @param <T> the type t
   * @param clazz the clazz
   * @return the table name
   */
  public static <T> String getTableName(final Class<T> clazz) {
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null) {
      if (StringUtils.nonEmpty(tableInfo.name())) {
        return tableInfo.name();
      }
    }

    final Entity entity = clazz.getAnnotation(Entity.class);
    if (entity != null && StringUtils.nonEmpty(entity.name())) {
      return entity.name();
    }
    return StringUtils.camelToUnderline(clazz.getSimpleName());
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
      throw CommonException.of("cannot get service bound type poJo.");
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
   */
  public static void main(String[] args) {
    log.info("main:[{}]", getAllColumnFields(Ts.class));
  }
}
