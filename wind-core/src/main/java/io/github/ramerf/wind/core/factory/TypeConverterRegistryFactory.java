package io.github.ramerf.wind.core.factory;

import io.github.ramerf.wind.core.converter.*;
import io.github.ramerf.wind.core.helper.TypeConverterHelper.ValueType;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.lang.reflect.Type;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 注册类型转换器.
 *
 * @author Tang Xiaofeng
 * @since 2020 /3/28
 */
@Slf4j
@SuppressWarnings({"rawtypes", "unused"})
public class TypeConverterRegistryFactory {
  private Set<TypeConverter> typeConverters =
      new TreeSet<>(((o1, o2) -> Objects.equals(o1.getClass(), o2.getClass()) ? 0 : 1));

  /** Instantiates a new Type converter registry. */
  public TypeConverterRegistryFactory() {}

  /** 注册默认的类型转换器,在添加自定义转换器之后添加这些,保持自定义的转换器优先级更高. */
  public void registerDefaultTypeConverters() {
    addTypeConverters(new BigDecimalTypeConverter());
    addTypeConverters(new BitSetTypeConverter());
    addTypeConverters(new DateTypeConverter());
    addTypeConverters(new EnumTypeConverter());
    addTypeConverters(new IntegerArrayTypeConverter());
    addTypeConverters(new ListIntegerArrayTypeConverter());
    addTypeConverters(new ListLongArrayTypeConverter());
    addTypeConverters(new ListStringArrayTypeConverter());
    addTypeConverters(new LongArrayTypeConverter());
    addTypeConverters(new StringArrayTypeConverter());
    addTypeConverters(new TimestampTypeConverter());
  }

  /**
   * 添加类型转换器.
   *
   * @param converters the {@link TypeConverter}
   * @see TypeConverter
   */
  public void addTypeConverters(@Nonnull TypeConverter... converters) {
    typeConverters.addAll(Arrays.asList(converters));
  }

  /**
   * Add type converter.
   *
   * @param converters the list of converter
   */
  public void addTypeConverter(@Nonnull Set<TypeConverter> converters) {
    CollectionUtils.doIfNonEmpty(converters, o -> typeConverters.addAll(converters));
  }

  /**
   * 设置类型转换器,将会覆盖默认的类型转换器.
   *
   * @param typeConverters the type converters
   */
  public void setTypeConverters(Set<TypeConverter> typeConverters) {
    this.typeConverters = typeConverters;
  }

  /**
   * Gets type converters.
   *
   * @return the type converters
   */
  public Set<TypeConverter> getTypeConverters() {
    return typeConverters;
  }

  /**
   * 获取Jdbc值转换为Java值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * <p>valueType {@link ValueType}
   *
   * @param valueType the value type
   * @return the type converter
   * @see TypeConverter
   */
  public TypeConverter getToJavaTypeConverter(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (Objects.isNull(value)) {
      return null;
    }
    final Type genericParameterType = valueType.getGenericParameterType();
    return getTypeConverters().stream()
        .filter(
            typeConverter -> {
              final Type javaClass = typeConverter.getJavaClass();
              final Type jdbcClass = typeConverter.getJdbcClass();
              try {
                return (Objects.equals(javaClass, genericParameterType)
                        || Class.forName(javaClass.getTypeName())
                            .isAssignableFrom(Class.forName(genericParameterType.getTypeName())))
                    && Objects.equals(value.getClass(), jdbcClass);
              } catch (ClassNotFoundException ignored) {
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }

  /**
   * 获取Java值转换为Jdbc值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * @param valueType {@link ValueType}
   * @return the type converter
   * @see TypeConverter
   */
  public TypeConverter getToJdbcTypeConverter(final ValueType valueType) {
    final Object value = valueType.getOriginVal();
    if (Objects.isNull(value)) {
      return null;
    }
    final Type genericParameterType = valueType.getGenericParameterType();
    return getTypeConverters().stream()
        .filter(
            typeConverter -> {
              final Type javaClass = typeConverter.getJavaClass();
              try {
                return Objects.equals(javaClass, genericParameterType)
                    || Class.forName(javaClass.getTypeName())
                        .isAssignableFrom(Class.forName(genericParameterType.getTypeName()));
              } catch (ClassNotFoundException ignored) {
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }
}
