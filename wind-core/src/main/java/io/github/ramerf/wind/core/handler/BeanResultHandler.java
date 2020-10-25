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
import java.lang.reflect.*;
import java.sql.Array;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cglib.proxy.*;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

/**
 * The type Bean result handler.
 *
 * @param <E> the type parameter
 * @since 2019 /12/27
 * @author Tang Xiaofeng
 */
@Slf4j
public class BeanResultHandler<E> extends AbstractResultHandler<Map<String, Object>, E> {
  private boolean bindProxy = true;

  /** 方法对应的字段. */
  private static final Map<Method, WeakReference<Field>> METHODS_FIELD_MAP =
      new ConcurrentHashMap<>();

  /**
   * Instantiates a new Bean result handler.
   *
   * @param clazz the clazz
   * @param queryColumns the query columns
   */
  public BeanResultHandler(@Nonnull final Class<E> clazz, final List<QueryColumn<?>> queryColumns) {
    super(clazz, queryColumns);
  }

  /**
   * Instantiates a new Bean result handler.
   *
   * @param clazz the clazz
   * @param queryColumns the query columns
   */
  public BeanResultHandler(@Nonnull final Class<E> clazz, final QueryColumn<?>... queryColumns) {
    super(clazz, queryColumns);
  }

  /**
   * Instantiates a new Bean result handler.
   *
   * @param clazz the clazz
   * @param bindProxy the bind proxy
   * @param queryColumns the query columns
   */
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
      final Class<?> paramType = method.getParameterTypes()[0];
      if (bindProxy && isPoJo) {
        if (AbstractEntityPoJo.class.isAssignableFrom(paramType)) {
          //noinspection unchecked
          initMappingObj(
              map,
              (AbstractEntityPoJo) obj,
              method,
              field,
              (Class<? extends AbstractEntityPoJo>) paramType,
              false);
          continue;
        } else
        // TODO-WARN 可能类型是集合
        if (Iterable.class.isAssignableFrom(paramType)) {
          Class<?> typeArgument =
              (Class<?>)
                  ((ParameterizedTypeImpl) method.getGenericParameterTypes()[0])
                      .getActualTypeArguments()[0];
          if (AbstractEntityPoJo.class.isAssignableFrom(typeArgument)) {
            //noinspection unchecked
            initMappingObj(
                map,
                (AbstractEntityPoJo) obj,
                method,
                field,
                (Class<? extends AbstractEntityPoJo>) typeArgument,
                true);
          }
          continue;
        }
        // TODO-WARN 关联查询
        // TODO-WARN 初始化关联对象
        // setMappingObject(map, obj, method, fieldName, field);
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
      final Class<?> parameterType = paramType;

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

  /**
   * @param map the map
   * @param obj the obj
   * @param method the method
   * @param field the field
   * @param paramType 关联对象类型
   */
  private <T extends AbstractEntityPoJo> void initMappingObj(
      final Map<String, Object> map,
      final T obj,
      final Method method,
      final Field field,
      final Class<? extends AbstractEntityPoJo> paramType,
      boolean isCollection) {
    final MappingInfo mappingInfo = EntityMapping.get(obj.getClass(), field).orElse(null);
    if (mappingInfo == null) {
      return;
    }
    final AbstractEntityPoJo mappingObj = BeanUtils.initial(paramType);
    final Field referenceField = mappingInfo.getReferenceField();
    referenceField.setAccessible(true);
    BeanUtils.setValue(mappingObj, referenceField, map.get(mappingInfo.getColumn()), null);
    if (isCollection) {
      final Class<?> type = method.getParameterTypes()[0];
      if (List.class.isAssignableFrom(type)) {
        BeanUtils.setValue(obj, field, Collections.singletonList(mappingObj), null);
      }
      if (Set.class.isAssignableFrom(type)) {
        BeanUtils.setValue(obj, field, Collections.singleton(mappingObj), null);
      }
    } else {
      BeanUtils.setValue(obj, field, mappingObj, null);
    }
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

  /**
   * The type Fetch mapping interceptor.
   *
   * @param <E> the type parameter
   * @since 2020.10.25
   * @author Tang Xiaofeng
   */
  public static class FetchMappingInterceptor<E> implements MethodInterceptor {
    private final BeanResultHandler<?> resultHandler;
    /** The Map. */
    final Map<String, Object> map;

    /**
     * Instantiates a new Fetch mapping interceptor.
     *
     * @param resultHandler the result handler
     * @param map the map
     */
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

  /**
   * The type Fetch mapping filter.
   *
   * @since 2020.10.25
   * @author Tang Xiaofeng
   */
  public static class FetchMappingFilter implements CallbackFilter {
    private final BeanResultHandler<?> resultHandler;
    /** The Map. */
    final Map<String, Object> map;

    /**
     * Instantiates a new Fetch mapping filter.
     *
     * @param resultHandler the result handler
     * @param map the map
     */
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
