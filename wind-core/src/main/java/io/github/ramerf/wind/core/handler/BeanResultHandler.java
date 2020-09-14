package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
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
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.bind.annotation.*;
import net.bytebuddy.matcher.ElementMatchers;

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
    E obj = BeanUtils.initial(clazz);

    // TODO-WARN ASM
    if (obj instanceof AbstractEntityPoJo) {
      obj = createByteBuddyDynamicProxy(clazz, obj);
    }
    // TODO-WARN ASM
    for (Method method : super.methods) {
      // TODO-WARN ASM
      if (method.getParameterTypes().length == 1
          && !BeanUtils.isPrimitiveType(method.getParameterTypes()[0])) {
        try {
          method.invoke(obj, (Object) null);
        } catch (IllegalAccessException e) {
          e.printStackTrace();
        } catch (InvocationTargetException e) {
          e.printStackTrace();
        }
      }
      // TODO-WARN ASM
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

  @SuppressWarnings("unchecked")
  private E createByteBuddyDynamicProxy(Class<E> clazz, E instance) {
    try {
      return (E)
          new ByteBuddy()
              .subclass(clazz)
              // .implement(AbstractEntity.class)
              .method(ElementMatchers.any().and(ElementMatchers.takesArguments(1)))
              .intercept(MethodDelegation.to(new SingerAgentInterceptor(instance)))
              .make()
              .load(BeanResultHandler.class.getClassLoader())
              .getLoaded()
              .newInstance();
    } catch (Exception e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return null;
    }
  }

  public static class SingerAgentInterceptor {
    private Object delegate;

    public SingerAgentInterceptor(Object delegate) {
      this.delegate = delegate;
    }

    /**
     * @param proxy 代理对象
     * @param method 代理方法
     * @param args 方法参数
     */
    @RuntimeType
    public Object intercept(
        @This Object proxy, @Origin Method method, @AllArguments Object[] args) {
      System.out.println("bytebuddy delegate proxy before sing " + method.getName());
      try {
        if (BeanUtils.isPrimitiveType(method.getParameterTypes()[0])) {
          return method.invoke(delegate, args);
        }
        // 注入
        log.info("intercept:[{}]", "注入");
        return method.invoke(delegate, args);
      } catch (Exception e) {
        log.warn(e.getMessage());
        log.error(e.getMessage(), e);
        return null;
      }
      // Object ret = method.invoke(delegate, args);
      // log.info("intercept:[{}]", args);
      // return null;
    }

    public Account getAccount() {
      if (account != null) {
        return account;
      }
      final Query query = Query.getInstance();
      final QueryColumn column = QueryColumnFactory.fromClass(Account.class);
      final IConsumer date = (IConsumer<Account, Long>) Account::setFooId;
      final Account account =
          query
              .select(column)
              .stringWhere(condition -> condition.eq("date", getId()))
              .fetchOne(Account.class);
      setAccount(account);
      return account;
    }
  }
}
