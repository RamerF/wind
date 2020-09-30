package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.mapping.*;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
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
  private boolean bindProxy = true;

  /** 方法对应的字段. */
  private static final Map<Method, WeakReference<Field>> METHODS_FIELD_MAP =
      new ConcurrentHashMap<>();

  public BeanResultHandler(@Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns);
  }

  public BeanResultHandler(@Nonnull final Class<E> clazz, final QueryColumn<?>... queryColumns) {
    super(clazz, queryColumns);
  }

  public BeanResultHandler(
      @Nonnull final Class<E> clazz, boolean bindProxy, final QueryColumn<?>... queryColumns) {
    super(clazz, queryColumns);
    this.bindProxy = bindProxy;
  }

  @Override
  public E handle(Map<String, Object> map) {
    // map = {alia:value}
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final E obj = initClazz(map);
    final boolean isPoJo = AbstractEntityPoJo.class.isAssignableFrom(clazz);
    for (Method method : super.methods) {
      final String fieldName = BeanUtils.methodToProperty(method.getName());
      final Field field = getField(method, fieldName);
      if (bindProxy
          && isPoJo
          && AbstractEntityPoJo.class.isAssignableFrom(method.getParameterTypes()[0])) {
        setMappingObject(map, obj, method, fieldName, field);
        continue;
      }

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
    // if (obj instanceof AbstractEntityPoJo) {
    //   // 关联对象设置代理
    //   EntityHelper.getEntityInfo(((AbstractEntityPoJo) obj).getClass())
    //       .getMappingInfos()
    //       .forEach(
    //           mappingInfo ->
    //               BeanUtils.setValue(
    //                   obj,
    //                   mappingInfo.getField(),
    //                   mappingInfo
    //                       .getMappingType()
    //                       .fetchMapping(
    //                           (AbstractEntityPoJo) obj,
    //                           mappingInfo,
    //                           map.get(mappingInfo.getColumn())),
    //                   null));
    // }
    return obj;
  }

  @SuppressWarnings("unchecked")
  private void setMappingObject(
      final Map<String, Object> map,
      final E obj,
      final Method method,
      final String fieldName,
      final Field field) {
    final Class<? extends AbstractEntityPoJo> mappingClass =
        (Class<? extends AbstractEntityPoJo>) method.getParameterTypes()[0];
    final Optional<MappingInfo> optional =
        EntityMapping.get((Class<? extends AbstractEntityPoJo>) clazz, getField(method, fieldName));
    if (!optional.isPresent()) {
      return;
    }
    final MappingInfo mappingInfo = optional.get();
    if (mappingInfo.getMappingType() == null) {
      return;
    }
    AbstractEntityPoJo mappingObject;
    final Object relationValue = map.get(mappingInfo.getColumn());
    if (ThreadLocalMappingFetch.isCached(mappingClass, relationValue)) {
      mappingObject = ThreadLocalMappingFetch.getObject(mappingClass, relationValue);
    } else {
      // 未缓存时,返回代理,并缓存代理的目标实例
      final AbstractEntityPoJo proxy =
          new OneToOneFetch((AbstractEntityPoJo) obj, mappingInfo, relationValue).getFetchProxy();
      // 如果id为null,说明未查询到数据
      if (proxy.getId() == null) {
        mappingObject = null;
      } else {
        OneToOneFetch.OneToOneLazyLoader.getEmptyPoJo(proxy.getClass());
        final AbstractEntityPoJo initial = BeanUtils.initial(mappingClass);
        BeanUtils.copyProperties(proxy, initial);
        mappingObject = initial;
      }
      ThreadLocalMappingFetch.putObject(mappingObject, relationValue);
    }
    BeanUtils.setValue(
        obj,
        field,
        mappingObject,
        e -> {
          log.warn(e.getMessage());
          log.error(e.getMessage(), e);
        });
  }

  /** 如果是AbstractEntityPoJo子类,返回代理,查询关联对象. */
  private E initClazz(final Map<String, Object> map) {
    final E obj;
    // if (AbstractEntityPoJo.class.isAssignableFrom(clazz) && fieldAliaMap.size() > 0) {
    //   Enhancer enhancer = new Enhancer();
    //   enhancer.setSuperclass(clazz);
    //   enhancer.setCallbacks(
    //       new Callback[] {new FetchMappingInterceptor<>(this, map), NoOp.INSTANCE});
    //   enhancer.setCallbackFilter(new FetchMappingFilter(this, map));
    //   @SuppressWarnings("unchecked")
    //   final E e = obj = (E) enhancer.create();
    // } else {
    //   obj = BeanUtils.initial(clazz);
    // }
    // return obj;
    return BeanUtils.initial(clazz);
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

  public static class FetchMappingInterceptor<E> implements MethodInterceptor {
    private final BeanResultHandler<?> resultHandler;
    final Map<String, Object> map;

    public FetchMappingInterceptor(
        final BeanResultHandler<?> resultHandler, final Map<String, Object> map) {
      this.resultHandler = resultHandler;
      this.map = map;
    }

    @Override
    public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) {
      AbstractEntityPoJo poJo = (AbstractEntityPoJo) obj;
      final Field field =
          resultHandler.getField(method, BeanUtils.methodToProperty(method.getName()));
      final Object relationValue = map.get(resultHandler.fieldAliaMap.get(field.getName()));
      // 查询关联字段
      log.debug("get relation:[{}]", method.getName());
      return EntityMapping.get(poJo.getClass(), field)
          .map(mappingInfo -> MappingType.of(field).fetchMapping(poJo, mappingInfo, relationValue))
          .orElse(null);
    }
  }

  public static class FetchMappingFilter implements CallbackFilter {
    private final BeanResultHandler<?> resultHandler;
    final Map<String, Object> map;

    public FetchMappingFilter(
        final BeanResultHandler<?> resultHandler, final Map<String, Object> map) {
      this.resultHandler = resultHandler;
      this.map = map;
    }

    @Override
    public int accept(final Method method) {
      final boolean isGetter = method.getName().startsWith("get");
      final boolean noArgs = method.getParameterTypes().length == 0;
      final boolean isPoJo = AbstractEntityPoJo.class.isAssignableFrom(method.getReturnType());
      if (!isGetter || !noArgs || !isPoJo) {
        return 1;
      }
      final Field field =
          resultHandler.getField(method, BeanUtils.methodToProperty(method.getName()));
      // 未标记为不抓取的字段
      if (!EntityUtils.isNotDontFetch(field)) {
        return 1;
      }
      final Object relationValue = map.get(resultHandler.fieldAliaMap.get(field.getName()));
      if (relationValue == null) {
        return 1;
      }
      return 0;
    }
  }
}
