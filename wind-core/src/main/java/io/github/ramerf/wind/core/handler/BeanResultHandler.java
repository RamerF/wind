package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.converter.TypeConverter;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Method;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.util.BeanUtils.methodToProperty;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/27
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {

  public BeanResultHandler(@Nonnull final Class<E> clazz) {
    super(clazz);
  }

  /**
   * {@inheritDoc}<br>
   * 有风险! 为了优化性能,这里假设类{@code clazz}初始化后,如果查询数据库列为空,对应类的属性值也为空.
   */
  @SuppressWarnings({"rawtypes", "unchecked"})
  @Override
  public E handle(Map<String, Object> map) {
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    // map = {alia:value}
    final E obj = BeanUtils.initial(clazz);

    for (Method method : super.methods) {
      final boolean isWriteMethod =
          (method.getName().startsWith("set") || method.getName().startsWith("is"))
              && method.getParameterTypes().length > 0;
      if (!isWriteMethod) {
        continue;
      }
      final String fieldName = methodToProperty(method.getName());
      Object value = map.get(StringUtils.camelToUnderline(fieldName));
      // 如果是数据库数组类型,获取对应的java值
      if (value instanceof Array) {
        try {
          value = ((Array) value).getArray();
        } catch (SQLException e) {
          log.warn("handle:fail to get array[{}]", e.getMessage());
          log.error(e.getMessage(), e);
        }
      }

      // 有风险! 这里假设类初始化后,如果查询数据库列为空,类的属性值也为空
      if (Objects.isNull(value)) {
        continue;
      }
      // 判断数据类型,调用指定的转换器,获取到对应的Java值,如果没有就直接赋值.
      TypeConverter typeConverter =
          AppContextInject.getBean(TypeConverterRegistryFactory.class)
              .getToJavaTypeConverter(value, method.getGenericParameterTypes()[0]);
      final Class<?> parameterType = method.getParameterTypes()[0];
      if (Objects.nonNull(typeConverter)) {
        value = typeConverter.covertFromJdbc(value, parameterType);
      }
      final Object finalValue = value;
      BeanUtils.invoke(obj, method, value)
          .ifPresent(
              exception ->
                  log.warn(
                      "mapToBean:跳过类型不匹配的字段[{} {}->{}]",
                      fieldName,
                      parameterType.getSimpleName(),
                      Optional.ofNullable(finalValue)
                          .map(Object::getClass)
                          .map(Class::getSimpleName)
                          .orElse(null)));
    }
    return obj;
  }

  @Override
  public List<E> handle(List<Map<String, Object>> maps) {
    if (CollectionUtils.isEmpty(maps)) {
      return Collections.emptyList();
    }
    return maps.stream().map(this::handle).collect(Collectors.toList());
  }
}
