package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cglib.proxy.*;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/27
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {
  /** 方法对应的字段. */
  private static final Map<Method, WeakReference<Field>> METHODS_FIELD_MAP =
      new ConcurrentHashMap<>();

  public BeanResultHandler(@Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns);
  }

  @Override
  public E handle(Map<String, Object> map) {
    // map = {alia:value}
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final E obj;
    // 如果是poJo子类,返回代理
    if (AbstractEntityPoJo.class.isAssignableFrom(clazz)) {
      Enhancer enhancer = new Enhancer();
      enhancer.setSuperclass(clazz);
      enhancer.setCallback(new FetchMethodInterceptor(this));
      obj = (E) enhancer.create();
    } else {
      obj = BeanUtils.initial(clazz);
    }
    for (Method method : super.methods) {
      final String fieldName = BeanUtils.methodToProperty(method.getName());
      final String columnAlia = fieldAliaMap.get(fieldName);
      Object value =
          Optional.ofNullable(map.get(columnAlia))
              .orElseGet(() -> map.get(StringUtils.camelToUnderline(fieldName)));
      if (Objects.isNull(value)) {
        continue;
      }
      // 如果是数据库数组类型,获取对应的java数组
      if (value instanceof Array) {
        try {
          value = ((Array) value).getArray();
        } catch (SQLException e) {
          log.warn("handle:fail to get array[{}]", e.getMessage());
          log.error(e.getMessage(), e);
        }
      }

      // 判断数据类型,调用指定的转换器,获取到对应的Java值,如果没有就直接赋值.
      final Class<?> parameterType = method.getParameterTypes()[0];
      final Field field = getField(method, fieldName);
      final Object finalValue =
          TypeHandlerHelper.toJavaValue(
              ValueType.of(value, method.getGenericParameterTypes()[0], field), parameterType);
      BeanUtils.invoke(obj, method, finalValue)
          .ifPresent(
              exception ->
                  log.warn(
                      "handle:跳过类型不匹配的字段[fieldName:{},paramType:{},valueType:{}]",
                      fieldName,
                      parameterType.getSimpleName(),
                      Optional.ofNullable(finalValue)
                          .map(Object::getClass)
                          .map(Class::getSimpleName)
                          .orElse(null)));
    }
    return obj;
  }

  private Field getField(final Method method, final String fieldName) {
    return Optional.ofNullable(METHODS_FIELD_MAP.get(method))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final Field field = BeanUtils.getDeclaredField(clazz, fieldName);
              METHODS_FIELD_MAP.put(method, new WeakReference<>(field));
              return field;
            });
  }

  public static class FetchMethodInterceptor<E> implements MethodInterceptor {
    private final BeanResultHandler<?> resultHandler;

    public FetchMethodInterceptor(final BeanResultHandler<?> resultHandler) {
      this.resultHandler = resultHandler;
    }

    @Override
    public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy)
        throws Throwable {
      if (BeanUtils.isPrimitiveType(method.getReturnType())
          || !method.getName().contains("ccount")) {
        return proxy.invokeSuper(obj, args);
      }
      log.info("这里是对目标类进行增强:[{}]", method.getName());
      return getRelation(obj, method);
    }

    @SuppressWarnings("unchecked")
    public Object getRelation(Object obj, final Method method) {
      if (!(obj instanceof AbstractEntityPoJo)) {
        return obj;
      }

      AbstractEntityPoJo poJo = (AbstractEntityPoJo) obj;
      if (poJo.getId() == null) {
        log.info("getRelation:[{}]", "id为空");
        return null;
      }
      final Query query = Query.getInstance();
      final Class<?> type = method.getReturnType();
      final Field field =
          resultHandler.getField(method, BeanUtils.methodToProperty(method.getName()));
      final Object o =
          query
              .select(QueryColumnFactory.fromClass((Class<AbstractEntityPoJo>) type))
              .stringWhere(condition -> condition.eq(field, poJo.getId()))
              .fetchOne(type);
      log.info("getRelation:[{}]", o);
      return o;
    }
  }
}
