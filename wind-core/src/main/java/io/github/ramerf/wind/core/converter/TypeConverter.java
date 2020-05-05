package io.github.ramerf.wind.core.converter;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 数据库与Java Bean相互转换.暂时只支持单类双向,如果双向转换不存在一个类中,建议直接弃用. 😀<br>
 * 注意: 可能会考虑使用注解,标记了注解的会使用指定的转换器<br>
 * 用法: @TypeConverter(EnumTypeConverter)
 *
 * @param <T> Java对象类型
 * @param <V> 数据库值类型
 * @author Tang Xiaofeng
 * @since 2020 /3/4
 */
@SuppressWarnings({"rawtypes", "unused"})
public interface TypeConverter<T, V> {
  /**
   * The constant log.
   */
  Logger log = LoggerFactory.getLogger(TypeConverter.class);

  /**
   * The constant PARAM_TYPE_CLAZZ.
   */
  Map<Class<?>, WeakReference<Type[]>> PARAM_TYPE_CLAZZ = new HashMap<>();

  /**
   * Java对象值转换为数据库值.
   *
   * @param t Java对象实例
   * @return 数据库值 v
   */
  V convertToJdbc(final T t);

  /**
   * 数据库值转换为Java对象值
   *
   * @param value 数据库值
   * @param clazz Java对象类型
   * @return Java对象值 t
   */
  T covertFromJdbc(final V value, final Class<? extends T> clazz);

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
  default Type getJavaClass() {
    return getParamTypeClass()[0];
  }

  /**
   * 获取jdbc类型.
   *
   * @return the jdbc class
   */
  default Type getJdbcClass() {
    return getParamTypeClass()[1];
  }

  /**
   * 获取泛型参数.
   *
   * @return the class [ ]
   */
  default Type[] getParamTypeClass() {
    Class<? extends TypeConverter> clazz = this.getClass();
    Type[] types = Optional.ofNullable(PARAM_TYPE_CLAZZ.get(clazz)).map(Reference::get).orElse(new Type[2]);
    if (Objects.nonNull(types[0])) {
      return types;
    }
    ParameterizedType parameterizedType = (ParameterizedType) clazz.getGenericInterfaces()[0];
    types = parameterizedType.getActualTypeArguments();
    PARAM_TYPE_CLAZZ.put(clazz, new WeakReference<>(types));
    return types;
  }
}
