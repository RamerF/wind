package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.InterService;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.framework.AdvisedSupport;
import org.springframework.aop.framework.AopProxy;
import org.springframework.aop.support.AopUtils;

import static io.github.ramerf.wind.core.util.BeanUtils.isPrimitiveType;
import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * The type Bean utils.
 *
 * @author ramer
 * @since 2019 /12/26
 */
@Slf4j
public final class EntityUtils {
  private static WindConfiguration configuration;
  private static Dialect dialect;
  /** {@link BaseService} */
  private static final Map<Class<?>, WeakReference<Class<?>>> SERVICE_POJO_MAP =
      new ConcurrentHashMap<>();

  public static void initial(final WindContext context) {
    EntityUtils.configuration = context.getWindConfiguration();
    EntityUtils.dialect = context.getDbMetaData().getDialect();
  }

  /**
   * 获取对象映射到数据库的属性,包括关系属性.<br>
   *
   * @param obj the obj
   * @return the non null field
   */
  public static List<Field> getAllColumnFields(@Nonnull final Class<?> obj) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(obj, ArrayList::new).stream()
            .filter(EntityUtils::filterColumnField)
            .collect(toList());
    if (log.isTraceEnabled()) {
      log.trace("getAllColumnFields:[{}]", fields);
    }
    return fields;
  }

  /**
   * 列必须符合以下条件:
   * <li>非static
   * <li>非transient
   * <li>基本类型(对应的包装类型)<br>
   *
   *     <p>或 AbstractEntityPoJo的子类并且标记有注解({@link OneToOne},{@link OneToMany},{@link ManyToOne})中的一个
   *     <br>
   *
   *     <p>或 List{@code <T>}/Set{@code <T>},T满足上一个条件
   */
  private static boolean filterColumnField(Field field) {
    final int modifiers = field.getModifiers();
    return !Modifier.isStatic(modifiers)
        && !Modifier.isTransient(modifiers)
        && (isPrimitiveType(field.getType())
            || MappingInfo.isValidMapping(field)
            || dialect.isSupportJavaType(field.getGenericType())
            || field.isAnnotationPresent(TableColumn.class));
  }

  /**
   * 获取对象映射到数据库且值不为null的属性.<br>
   *
   * @param <T> the type parameter
   * @param t the t
   * @return the non null Field
   */
  public static <T> List<Field> getNonNullColumnFields(@Nonnull final T t) {
    @SuppressWarnings("unchecked")
    final List<Field> fields =
        // BeanUtils.retrievePrivateFields(t.getClass(), ArrayList::new).stream()
        //     .filter(EntityUtils::filterColumnField)
        EntityHelper.getEntityInfo(t.getClass()).getEntityColumns().stream()
            .map(EntityColumn::getField)
            .filter(field -> Objects.nonNull(BeanUtils.getValue(t, field, null)))
            .collect(toList());
    if (log.isTraceEnabled()) {
      log.debug("getNonNullColumnFields:[{}]", fields);
    }
    // return filterCustomField(t, fields);
    return fields;
  }

  /*
  // private static <T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
  //     List<Field> filterCustomField(@Nonnull final T t, final List<Field> fields) {
  //   @SuppressWarnings("unchecked")
  //   final Class<T> clazz = (Class<T>) t.getClass();
  //   final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
  //   // 剔除掉自定义字段
  //   Stream<Field> stream = fields.stream();
  //   // 创建时间
  //   final Field createTimeField = entityInfo.getCreateTimeField();
  //   if (createTimeField != null) {
  //     // 可能覆盖父类的字段
  //     stream =
  //         stream.filter(
  //             field ->
  //                 !CREATE_TIME_FIELD_NAME.equals(field.getName()) ||
  // field.equals(createTimeField));
  //   }
  //   // 更新时间
  //   final Field updateTimeField = entityInfo.getUpdateTimeField();
  //   if (updateTimeField != null) {
  //     stream =
  //         stream.filter(
  //             field ->
  //                 !UPDATE_TIME_FIELD_NAME.equals(field.getName()) ||
  // field.equals(updateTimeField));
  //   }
  //   // 逻辑删除
  //   final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
  //   // 未启用 或者 自定义字段
  //   if (!logicDeleteProp.isEnable()
  //       || !LOGIC_DELETE_COLUMN_NAME.equals(logicDeleteProp.getColumn())) {
  //     stream = stream.filter(field -> !LOGIC_DELETE_FIELD_NAME.equals(field.getName()));
  //   }
  //   return stream.collect(toList());
  // }
  */

  /**
   * 获取对象映射到数据库且值为null的属性.
   *
   * @param t the t
   * @return the non null field
   */
  public static List<Field> getNullColumnFields(@Nonnull final Object t) {
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(t.getClass(), ArrayList::new).stream()
            .filter(EntityUtils::filterColumnField)
            .filter(field -> Objects.isNull(BeanUtils.getValue(t, field, ex -> -1)))
            .collect(toList());
    log.debug("getNullColumnFields:[{}]", fields);
    return fields;
  }

  /**
   * 获取对象映射到数据库的列名.<br>
   * 默认值为{@link TableColumn#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param obj the obj
   * @return string all columns
   * @see TableColumn#name() Column#name()
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
   * 获取对象映射到数据库的非空属性的列名.<br>
   * 默认值为{@link TableColumn#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param t the t
   * @return string non null columns
   * @see TableColumn#name() Column#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static <ID extends Serializable> List<String> getNonNullColumns(@Nonnull final Object t) {
    final List<String> columns =
        getNonNullColumnFields(t).stream().map(EntityUtils::fieldToColumn).collect(toList());
    log.debug("getNonNullColumns:[{}]", columns);
    return columns;
  }

  /**
   * 获取对象映射到数据库的非空属性的列名,以逗号分割.<br>
   * 默认值为{@link TableColumn#name()};如果前者为空,值为对象属性名的下划线表示<br>
   * {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   *
   * @param t the t
   * @return string non null column
   * @see TableColumn#name() Column#name()
   * @see StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   * @see #getNonNullColumns(Object)
   */
  public static <ID extends Serializable> String getNonNullColumn(@Nonnull final Object t) {
    final String nonNullColumn = String.join(",", getNonNullColumns(t));
    log.debug("getNonNullColumn:[{}]", nonNullColumn);
    return nonNullColumn;
  }

  /**
   * 获取对象属性对应的数据库列名.<br>
   * <li>普通字段:默认值为{@link TableColumn#name()};如果前者为空,值为<br>
   *     {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   * <li>N对1关联字段:
   *
   * @param field the field
   * @return string string
   * @see TableColumn#name() TableColumn#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static String fieldToColumn(@Nonnull final Field field) {
    final Class<?> fieldType = field.getType();
    // 普通字段判断TableColumn注解
    if (!BeanUtils.isPrimitiveType(fieldType) || !MappingInfo.isValidMapping(field)) {
      final TableColumn column = field.getAnnotation(TableColumn.class);
      return column != null && StringUtils.nonEmpty(column.name())
          ? column.name()
          : camelToUnderline(field.getName());
    }
    // 关联字段
    final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
    final String joinColumnName;
    final String reference;
    if (oneToOne != null) {
      joinColumnName = oneToOne.joinColumnName();
      reference = oneToOne.referenceField();
    } else {
      final ManyToOne manyToOne = field.getAnnotation(ManyToOne.class);
      joinColumnName = manyToOne.joinColumnName();
      reference = manyToOne.referenceField();
    }
    // 手动指定关联列名
    if (!"".equals(joinColumnName)) {
      return joinColumnName;
    }
    // 关联主键
    if ("".equals(reference)) {
      final String primaryKeys =
          EntityHelper.getEntityInfo(fieldType).getPrimaryKeys().stream()
              .map(EntityColumn::getName)
              .collect(joining("_"));
      return camelToUnderline(fieldType.getSimpleName().concat("_").concat(primaryKeys));
    }
    final Field referenceField = BeanUtils.getDeclaredField(fieldType, reference);
    if (referenceField == null) {
      throw CommonException.of("Invalid mapping field " + reference);
    }
    return camelToUnderline(
        fieldType.getSimpleName().concat("_").concat(fieldToColumn(referenceField)));
  }

  /**
   * 表名: {@link TableInfo#name()} &gt; 类名(驼封转下划线)
   *
   * @param clazz the clazz
   * @return the table name
   */
  public static String getTableName(final Class<?> clazz) {
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null && StringUtils.nonEmpty(tableInfo.name())) {
      return tableInfo.name();
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
  public static <T, S extends InterService<T, ID>, ID extends Serializable> Class<T> getPoJoClass(
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

    // TODO WARN 这里应该通过循环，获取pojo，如果类包含TableInfo注解就停止
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
