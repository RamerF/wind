package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.exception.NotImplementedException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.InterService;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;

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
  private static Configuration configuration;
  private static Dialect dialect;
  /** {@link BaseService} */
  private static final Map<Class<?>, WeakReference<Class<?>>> SERVICE_POJO_MAP =
      new ConcurrentHashMap<>();

  public static void initial(final WindContext context) {
    EntityUtils.configuration = context.getConfiguration();
    EntityUtils.dialect = context.getDbMetaData().getDialect();
  }

  public static List<Field> getAllColumnFields(@Nonnull final Class<?> obj) {
    return getAllColumnFields(obj, null);
  }

  /** TODO WARN 这里不应该包含关联属性，获取对象映射到数据库的属性,包括关系属性.<br> */
  public static List<Field> getAllColumnFields(
      @Nonnull final Class<?> obj, @Nullable SqlStatementType sqlStatementType) {
    Stream<Field> stream =
        BeanUtils.retrievePrivateFields(obj, ArrayList::new).stream()
            .filter(EntityUtils::filterColumnField);
    if (sqlStatementType != null) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo(obj);
      final Map<Field, EntityColumn> fieldColumnMap = entityInfo.getFieldColumnMap();
      if (sqlStatementType.equals(SqlStatementType.INSERT)) {
        stream = stream.filter(o -> fieldColumnMap.get(o).isInsertable());
      } else if (sqlStatementType.equals(SqlStatementType.UPDATE)) {
        stream = stream.filter(o -> fieldColumnMap.get(o).isUpdatable());
      }
    }
    final List<Field> fields = stream.collect(toList());
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
   *     <p>或 标记有注解({@link OneToOne},{@link OneToMany},{@link ManyToOne})中的一个 <br>
   *
   *     <p>或 List{@code <T>}/Set{@code <T>},T满足上一个条件
   */
  private static boolean filterColumnField(final Field field) {
    final int modifiers = field.getModifiers();
    // 不保存 static 和 transient 修饰的字段
    if (Modifier.isStatic(modifiers) || Modifier.isTransient(modifiers)) {
      return false;
    }
    if (field.isAnnotationPresent(TableColumn.class)) {
      return true;
    }
    if (dialect.isSupportJavaType(field.getGenericType())) {
      return true;
    }
    return InterEnum.class.isAssignableFrom(field.getType());
  }

  public enum SqlStatementType {
    INSERT,
    DELETE,
    UPDATE,
    SELECT
  }

  /** 获取对象映射到数据库且值不为null的属性. */
  public static <T> List<Field> getNonNullColumnFields(
      @Nonnull final T t, final SqlStatementType sqlStatementType) {
    Stream<Field> stream =
        EntityHelper.getEntityInfo(t.getClass()).getEntityColumns().stream()
            .map(EntityColumn::getField)
            .filter(EntityUtils::filterColumnField);
    if (sqlStatementType != null) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo(t.getClass());
      final Map<Field, EntityColumn> fieldColumnMap = entityInfo.getFieldColumnMap();
      if (sqlStatementType.equals(SqlStatementType.INSERT)) {
        stream = stream.filter(o -> fieldColumnMap.get(o).isInsertable());
      } else if (sqlStatementType.equals(SqlStatementType.UPDATE)) {
        stream = stream.filter(o -> fieldColumnMap.get(o).isUpdatable());
      }
    }
    final List<Field> fields =
        stream.filter(field -> BeanUtils.getValue(t, field, null) != null).collect(toList());
    if (log.isTraceEnabled()) {
      log.debug("getNonNullColumnFields:[{}]", fields);
    }
    return fields;
  }

  /**
   * 获取对象映射到数据库的列名.
   *
   * @see TableColumn#name() Column#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static List<String> getAllColumns(
      @Nonnull final Class<?> obj, final SqlStatementType sqlStatementType) {
    final List<String> columns =
        getAllColumnFields(obj, sqlStatementType).stream()
            .map(EntityUtils::fieldToColumn)
            .collect(toList());
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
  public static List<String> getNonNullColumns(
      @Nonnull final Object t, final SqlStatementType sqlStatementType) {
    final List<String> columns =
        getNonNullColumnFields(t, sqlStatementType).stream()
            .map(EntityUtils::fieldToColumn)
            .collect(toList());
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
   * @see #getNonNullColumns(Object,SqlStatementType)
   */
  public static String getNonNullColumn(
      @Nonnull final Object t, final SqlStatementType sqlStatementType) {
    final String nonNullColumn = String.join(",", getNonNullColumns(t, sqlStatementType));
    log.debug("getNonNullColumn:[{}]", nonNullColumn);
    return nonNullColumn;
  }

  /** {@link #fieldToColumn(Field, boolean)} */
  public static String fieldToColumn(@Nonnull final Field field) {
    return fieldToColumn(field, false);
  }

  /**
   * 获取对象属性对应的数据库列名.<br>
   * <li>普通字段:默认值为{@link TableColumn#name()};如果前者为空,值为<br>
   *     {@link StringUtils#camelToUnderline(String)},{@link Field#getName()}
   * <li>N对1关联字段:
   *
   * @param field the field
   * @param depth 是否解析关联对象
   * @return string string
   * @see TableColumn#name() TableColumn#name()
   * @see StringUtils#camelToUnderline(String) StringUtils#camelToUnderline(String)
   * @see Field#getName() Field#getName()
   */
  public static String fieldToColumn(@Nonnull final Field field, final boolean depth) {
    final Class<?> fieldType = field.getType();
    // 普通字段判断TableColumn注解
    if (!depth || field.getType().getClassLoader() == null || !MappingInfo.isValidMapping(field)) {
      final TableColumn column = field.getAnnotation(TableColumn.class);
      return column != null && !"".equals(column.name())
          ? column.name()
          : camelToUnderline(field.getName());
    }
    // 关联字段
    final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
    final String joinFieldStr;
    final String targetFieldStr;
    if (oneToOne != null) {
      joinFieldStr = oneToOne.joinField();
      targetFieldStr = oneToOne.targetField();
    } else {
      final ManyToOne manyToOne = field.getAnnotation(ManyToOne.class);
      if (manyToOne != null) {
        joinFieldStr = manyToOne.joinField();
        targetFieldStr = manyToOne.targetField();
      } else {
        return null;
      }
    }
    // 手动指定关联列名
    if (!"".equals(joinFieldStr)) {
      final Field joinField = BeanUtils.getDeclaredField(fieldType, joinFieldStr);
      if (joinField == null) {
        throw new IllegalArgumentException(
            String.format(
                "%s %s's join field could not null", fieldType.getName(), field.getName()));
      }
      return fieldToColumn(joinField, false);
    }
    // 关联主键
    final EntityInfo entityInfo = EntityHelper.getEntityInfo(fieldType);
    if ("".equals(targetFieldStr)) {
      final String primaryKeys =
          entityInfo.getPrimaryKeys().stream().map(EntityColumn::getName).collect(joining("_"));
      return camelToUnderline(entityInfo.getName().concat("_").concat(primaryKeys));
    }
    final Field targetField =
        entityInfo.getFieldColumnMap().keySet().stream()
            .filter(f -> f.getName().equals(targetFieldStr))
            .findFirst()
            .orElse(null);
    if (targetField == null) {
      throw new IllegalArgumentException(
          String.format("Invalid target field %s for %s", targetFieldStr, fieldType));
    }
    final String column = fieldToColumn(targetField, false);
    return column == null
        ? null
        : camelToUnderline(entityInfo.getName().concat("_").concat(column));
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

  @SuppressWarnings("unchecked")
  public static <T, S extends InterService<T, ID>, ID extends Serializable> Class<T> getPoJoClass(
      S service) {
    return getPoJoClass((Class<S>) getProxyTarget(service).getClass());
  }

  /** 获取Service泛型参数poJo. */
  @SuppressWarnings("unchecked")
  public static <T, S extends InterService<T, ID>, ID extends Serializable> Class<T> getPoJoClass(
      Class<S> serviceClazz) {
    Class<T> classes =
        (Class<T>)
            Optional.ofNullable(SERVICE_POJO_MAP.get(serviceClazz))
                .map(Reference::get)
                .orElse(null);
    if (classes != null) {
      return classes;
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    final Type[] baseServiceTypes =
        Stream.of(serviceClazz.getInterfaces())
            .filter(InterService.class::isAssignableFrom)
            .findFirst()
            .get()
            .getGenericInterfaces();
    for (Type baseServiceType : baseServiceTypes) {
      if (baseServiceType instanceof ParameterizedType) {
        ParameterizedType parameterizedType = (ParameterizedType) baseServiceType;
        final Type[] arguments = parameterizedType.getActualTypeArguments();
        try {
          classes = (Class<T>) Class.forName(arguments[0].getTypeName());
        } catch (ClassNotFoundException ignored) {
          throw new CommonException("cannot get service bound type poJo.");
        }
        SERVICE_POJO_MAP.put(serviceClazz, new WeakReference<>(classes));
        return classes;
      }
    }
    Class<S> cls;
    try {
      cls = (Class<S>) BeanUtils.getTypeClass(baseServiceTypes[0]);
    } catch (ClassCastException e) {
      throw new CommonException("cannot get service bound type poJo.");
    }
    return getPoJoClass(cls);
  }

  /**
   * 获取代理对象的目标对象.
   *
   * @param proxy 代理对象
   */
  private static Object getProxyTarget(Object proxy) {
    // if (!AopUtils.isAopProxy(proxy)) {
    //   return proxy;
    // }
    // if (AopUtils.isJdkDynamicProxy(proxy)) {
    //   try {
    //     return getJdkDynamicProxyTargetObject(proxy);
    //   } catch (Exception e) {
    //     log.warn(e.getMessage());
    //     log.error(e.getMessage(), e);
    //   }
    // } else {
    //   try {
    //     return getCglibProxyTargetObject(proxy);
    //   } catch (Exception e) {
    //     log.warn(e.getMessage());
    //     log.error(e.getMessage(), e);
    //   }
    // }
    // return getProxyTarget(proxy);
    throw new NotImplementedException("getCglibProxyTargetObject");
  }

  private static Object getCglibProxyTargetObject(Object proxy) throws Exception {
    // Field field = proxy.getClass().getDeclaredField("CGLIB$CALLBACK_0");
    // field.setAccessible(true);
    // Object dynamicAdvisedInterceptor = field.get(proxy);
    // Field advised = dynamicAdvisedInterceptor.getClass().getDeclaredField("advised");
    // advised.setAccessible(true);
    // return ((AdvisedSupport)
    // advised.get(dynamicAdvisedInterceptor)).getTargetSource().getTarget();
    throw new NotImplementedException("getCglibProxyTargetObject");
  }

  private static Object getJdkDynamicProxyTargetObject(Object proxy) throws Exception {
    // Field h = proxy.getClass().getSuperclass().getDeclaredField("h");
    // h.setAccessible(true);
    // AopProxy aopProxy = (AopProxy) h.get(proxy);
    // Field advised = aopProxy.getClass().getDeclaredField("advised");
    // advised.setAccessible(true);
    // return ((AdvisedSupport) advised.get(aopProxy)).getTargetSource().getTarget();
    throw new NotImplementedException("getJdkDynamicProxyTargetObject");
  }

  public static void main(String[] args) {
    getAllColumnFields(Ts.class).forEach(o -> System.out.println(o.getName()));
  }
}
