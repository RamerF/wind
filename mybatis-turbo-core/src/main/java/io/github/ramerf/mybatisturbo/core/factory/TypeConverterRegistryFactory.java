package io.github.ramerf.mybatisturbo.core.factory;

import io.github.ramerf.mybatisturbo.core.handler.*;
import java.lang.reflect.Type;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 注册类型转换器.
 *
 * @author Tang Xiaofeng
 * @since 2020 /3/28
 */
@Slf4j
@Component
@SuppressWarnings({"rawtypes", "unused"})
public class TypeConverterRegistryFactory {
  private List<TypeConverter> typeConverters = new ArrayList<>();

  /**
   * Instantiates a new Type converter registry.
   */
  public TypeConverterRegistryFactory() {
    initDefaultTypeConverters();
  }

  /**
   * 初始化默认的类型转换器.
   */
  private void initDefaultTypeConverters() {
    addTypeConverters(new EnumTypeConverter());
    addTypeConverters(new BigDecimalTypeConverter());
    addTypeConverters(new ListLongArrayTypeConverter());
    addTypeConverters(new ListStringArrayTypeConverter());
  }

  /**
   * 添加类型转换器.使用方式:
   *
   * <pre>
   * </pre>
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
   * @param converter the converter
   */
  public void addTypeConverter(@Nonnull TypeConverter converter) {
    typeConverters.add(converter);
  }

  /**
   * 设置类型转换器,将会覆盖默认的类型转换器.
   *
   * @param typeConverters the type converters
   */
  public void setTypeConverters(List<TypeConverter> typeConverters) {
    this.typeConverters = typeConverters;
  }

  /**
   * Gets type converters.
   *
   * @return the type converters
   */
  public List<TypeConverter> getTypeConverters() {
    return typeConverters;
  }

  /**
   * 获取Jdbc值转换为Java值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * @param value         jdbc查询值
   * @param parameterType java类型
   * @return the type converter
   * @see TypeConverter
   */
  public TypeConverter getToJavaTypeConverter(final Object value, final Type parameterType) {
    if (Objects.isNull(value)) {
      return null;
    }
    return getTypeConverters().stream().filter(typeConverter -> {
      final Type javaClass = typeConverter.getJavaClass();
      final Type jdbcClass = typeConverter.getJdbcClass();
      try {
        return (Objects.equals(javaClass, parameterType) || Class.forName(javaClass.getTypeName())
            .isAssignableFrom(Class.forName(parameterType.getTypeName()))) && Objects
            .equals(value.getClass(), jdbcClass);
      } catch (ClassNotFoundException ignored) {
        // 如果这里抛出异常,就是程序的BUG,所以不需要处理
      }
      return false;
    }).findFirst().orElse(null);
  }

  /**
   * 获取Java值转换为Jdbc值类型转换器,用于将数据库值转换为Java类型.<br>
   * 后面可能会再添加一个Class/Field参数(用于获取字段上的转换器注解)
   *
   * @param value         java值
   * @param parameterType java类型
   * @return the type converter
   * @see TypeConverter
   */
  public TypeConverter getToJdbcTypeConverter(final Object value, final Type parameterType) {
    if (Objects.isNull(value)) {
      return null;
    }
    return getTypeConverters().stream().filter(typeConverter -> {
      try {
        return Objects.equals(typeConverter.getJavaClass(), parameterType) || Class
            .forName(typeConverter.getJavaClass().getTypeName())
            .isAssignableFrom(Class.forName(parameterType.getTypeName()));
      } catch (ClassNotFoundException ignored) {
        // 如果这里抛出异常,就是程序的BUG,所以不需要处理
      }
      return false;
    }).findFirst().orElse(null);
  }
}
