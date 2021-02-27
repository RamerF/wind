package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.sql.Types;
import java.util.*;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 数据库与Java类型之间转换.可以在字段上添加{@link TypeHandler}指定使用的类型转换器.
 *
 * @param <T> Java对象类型
 * @param <V> 数据库值类型
 * @since 2020 /3/4
 * @author ramer
 */
@SuppressWarnings({"rawtypes", "unused"})
public interface ITypeHandler<T, V> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(ITypeHandler.class);

  /** The constant PARAM_TYPE_CLAZZ. */
  Map<Class<?>, WeakReference<Type[]>> PARAM_TYPE_CLAZZ = new HashMap<>();

  /**
   * Java对象值转换为数据库值.
   *
   * @param t Java对象实例
   * @param field the field
   * @param ps {@link PreparedStatement}
   * @return 数据库值 v
   */
  Object convertToJdbc(final T t, final Field field, @Nonnull final PreparedStatement ps);

  /**
   * 数据库值转换为Java对象值<br>
   *
   * @param jdbcVal 数据库值
   * @param defaultValue java实例该字段的默认值
   * @param field 字段
   * @return Java对象值 t
   */
  T convertFromJdbc(final V jdbcVal, final Object defaultValue, final Field field);

  /**
   * 是否处理null值,默认处理.<br>
   * 注意: 当前该方法配置无效
   *
   * @return the boolean
   */
  default boolean convertNull() {
    return true;
  }

  /**
   * 获取Java类型.
   *
   * @return the java class
   */
  default @Nonnull Type getJavaClass() {
    return getParamTypeClass()[0];
  }

  /**
   * 获取jdbc类型.
   *
   * @return the jdbc class
   */
  default @Nonnull Type getJdbcClass() {
    return getParamTypeClass()[1];
  }

  /**
   * 获取jdbcType.<br>
   *
   * <pre>
   *    示例:
   *    return EntityHelper.getJdbcTypeName(field, "varchar");
   *  </pre>
   *
   * @param field the field
   * @return jdbcType名称
   * @see Types
   * @see EntityHelper#getJdbcTypeName(Field, String)
   */
  String getJdbcType(@Nonnull final Field field);

  /**
   * 获取泛型参数.
   *
   * @return the class [ ]
   */
  default Type[] getParamTypeClass() {
    Class<? extends ITypeHandler> clazz = this.getClass();
    Type[] types =
        Optional.ofNullable(PARAM_TYPE_CLAZZ.get(clazz)).map(Reference::get).orElse(new Type[2]);
    if (Objects.nonNull(types[0])) {
      return types;
    }
    ParameterizedType parameterizedType = (ParameterizedType) clazz.getGenericInterfaces()[0];
    types = parameterizedType.getActualTypeArguments();
    PARAM_TYPE_CLAZZ.put(clazz, new WeakReference<>(types));
    return types;
  }
}
