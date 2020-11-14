package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.UpdateService.Fields;
import io.github.ramerf.wind.core.util.CollectionUtils;
import io.github.ramerf.wind.core.util.PageUtils;
import java.util.List;
import java.util.Objects;
import java.util.function.*;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;

import static io.github.ramerf.wind.core.entity.response.Rs.*;
import static io.github.ramerf.wind.core.util.BeanUtils.initial;
import static io.github.ramerf.wind.core.util.EntityUtils.getPoJoClass;
import static io.github.ramerf.wind.core.validation.ValidateUtil.collect;

/**
 * 简化Controller操作.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
public final class ControllerHelper {
  /**
   * 执行创建.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<Object> create(
      final S invoke, final T entity) {
    return Rs.ok(json().put("id", invoke.create(entity)));
  }

  /**
   * Create response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param bindingResult the binding result
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<Object> create(
      final S invoke, final T entity, final BindingResult bindingResult) {
    log.info("create:[{}]", entity);
    if (bindingResult.hasErrors()) {
      return fail(collect(bindingResult));
    }
    return ok(json().put("id", invoke.create(entity)), ResultCode.API_SUCCESS_EXEC_CREATE.desc());
  }

  /**
   * Create response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param <P> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param bindingResult the binding result
   * @param fieldsConsumer 指定或排除保存/更新的字段
   * @return the response entity
   */
  public static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest<T>,
          P>
      Rs<P> create(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final Consumer<Fields<T>> fieldsConsumer) {
    return createOrUpdate(invoke, entity, bindingResult, true, fieldsConsumer);
  }

  /**
   * 通过Id获取实体详情.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param invoke the service
   * @param id the id
   * @return the entity
   * @see #detail(BaseService, long, Function) #detail(BaseService, long, Function)
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<Object> detail(
      final S invoke, final long id) {
    return detail(invoke, id, null);
  }

  /**
   * 通过Id获取实体详情.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the service
   * @param id the id
   * @param function 对查询到的数据执行额外的处理,例:<br>
   *     <code>      poJo -&gt; {        poJo.setPassword(null);      }     </code>
   * @return the entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R> Rs<R> detail(
      final S invoke, final long id, Function<T, R> function) {
    if (id < 1) {
      return wrongFormat("id");
    }
    final T t = invoke.getById(id);
    return Objects.nonNull(function) ? ok(function.apply(t)) : ok(t);
  }

  /**
   * 转换分页对象.将Page PoJo对象转换为 Page 任意对象,并封装为页面响应对象.
   *
   * @param <T> poJo对象
   * @param <R> the type parameter
   * @param page {@link Page} poJo对象
   * @param function 转换函数表达式
   * @return {@link ResponseEntity}
   */
  public static <T extends AbstractEntity, R> Rs<Page<R>> page(
      final Page<T> page, final Function<T, R> function) {
    return ok(PageUtils.toPage(page, function, null));
  }

  /**
   * Update response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param id the id
   * @param bindingResult the binding result
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R> Rs<R> update(
      final S invoke, final T entity, final long id, final BindingResult bindingResult) {
    if (id < 1) {
      return wrongFormat("id");
    }
    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return fail(collect(bindingResult));
    }
    entity.setId(id);
    final int update = invoke.update(entity);
    return update == 1
        ? notExist(String.valueOf(id))
        : ok(json().put("id", entity.getId()), ResultCode.API_SUCCESS_EXEC_UPDATE.desc());
  }

  /**
   * 执行更新.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @return the response entity
   * @see #update(BaseService, AbstractEntityPoJo, ResultCode, ResultCode) #update(BaseService,
   *     AbstractEntityPoJo, ResultCode, ResultCode)
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R> Rs<String> update(
      final S invoke, final T entity) {
    return update(invoke, entity, null, null);
  }

  /**
   * 执行更新.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   * @see #update(BaseService, AbstractEntityPoJo, ResultCode, ResultCode)
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<String> update(
      final S invoke, final T entity, final ResultCode errorCode) {
    return update(invoke, entity, null, errorCode);
  }

  /**
   * 执行更新.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param successCode 执行成功时的响应码,可以为null
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<String> update(
      final S invoke, final T entity, final ResultCode successCode, final ResultCode errorCode) {
    final int update = invoke.update(entity);
    return update == 1
        ? fail(ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST)
        : Objects.nonNull(successCode)
            ? ok(json().put("id", entity.getId()), successCode)
            : ok(json().put("id", entity.getId()));
  }

  /**
   * Update response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param id the id
   * @param bindingResult the binding result
   * @param fieldsConsumer 指定或排除保存/更新的字段
   * @return the response entity
   */
  public static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest<T>>
      Rs<Object> update(
          final S invoke,
          final R entity,
          final long id,
          final BindingResult bindingResult,
          final Consumer<Fields<T>> fieldsConsumer) {
    if (id < 1) {
      return wrongFormat("id");
    }
    entity.setId(id);
    return createOrUpdate(invoke, entity, bindingResult, false, fieldsConsumer);
  }

  /**
   * Delete response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param id the id
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R> Rs<R> delete(
      final S invoke, final long id) {
    if (id < 1) {
      return wrongFormat("id");
    }
    invoke.delete(id);
    return ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  /**
   * Delete batch response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param invoke the invoke
   * @param ids the ids
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo> Rs<String> deleteByIds(
      final S invoke, final List<Long> ids) {
    invoke.deleteByIds(ids);
    return ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  /**
   * 转换集合对象.将List poJo对象转换为 List 任意对象,并封装为页面响应对象.
   *
   * @param <T> poJo对象
   * @param <E> 任意对象(通常是response|poJo对象)
   * @param list List poJo对象
   * @param function 转换函数表达式
   * @param filterFunction 过滤函数表达式
   * @return {@link ResponseEntity}
   */
  public static <T extends AbstractEntity, E> Rs<List<E>> list(
      final List<T> list, final Function<T, E> function, final Predicate<E> filterFunction) {
    return ok(CollectionUtils.toList(list, function, filterFunction));
  }

  /**
   * 将 {@link List} 转换为 {@link Page}.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param list the list
   * @return the response entity
   */
  public static <T extends AbstractEntity, R> Rs<Page<R>> page(final List<T> list) {
    return ok(PageUtils.toPage(list));
  }

  /**
   * 将 {@link List} 转换为 {@link Page}
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param page the page
   * @param function the function
   * @param filterFunction the filter function
   * @return the response entity
   */
  public static <T extends AbstractEntity, R> Rs<Page<R>> page(
      final List<T> page, final Function<T, R> function, final Predicate<R> filterFunction) {
    return ok(PageUtils.toPage(page, function, filterFunction));
  }

  /**
   * 创建或更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的request {@link AbstractEntityRequest} 对象.
   * @param bindingResult 校验器校验结果.
   * @param create 是否是创建.
   * @param fieldsConsumer 指定或排除保存/更新的字段
   * @param <T> 服务层实现类.
   * @return {@link ResponseEntity}
   */
  private static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest<T>,
          P>
      Rs<P> createOrUpdate(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final boolean create,
          @Nonnull final Consumer<Fields<T>> fieldsConsumer) {
    if (log.isDebugEnabled()) {
      // 打印参数,处理不同层级调用的情况
      Stream.of(Thread.currentThread().getStackTrace())
          .filter(o -> o.getClassName().contains("controller."))
          .findFirst()
          .ifPresent(stack -> log.info("\n\t{}:[entity:{}]", stack, entity));
    }
    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return fail(collect(bindingResult));
    }
    final Long id = entity.getId();
    // 创建对象时id置为空
    if (create) {
      entity.setId(null);
    } else if (Objects.isNull(id) || id < 1) {
      return wrongFormat("id");
    }
    T poJo = initial(getPoJoClass(invoke));
    BeanUtils.copyProperties(entity, poJo);
    // 额外处理,比如敏感词过滤
    entity.redundantValue(poJo);
    log.debug("createOrUpdate:[{}]", poJo);
    long affectRow =
        create
            ? invoke.create(poJo, fieldsConsumer) > 0 ? 1 : 0
            : invoke.update(poJo, fieldsConsumer);
    return affectRow == 1
        ? ok(
            json().put("id", poJo.getId()),
            create
                ? ResultCode.API_SUCCESS_EXEC_CREATE.desc()
                : ResultCode.API_SUCCESS_EXEC_UPDATE.desc())
        : fail(
            create
                ? ResultCode.API_FAIL_EXEC_CREATE.desc()
                : ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST.desc());
  }

  private static <R> Rs<R> errorResponse(Exception e) {
    log.warn(
        "errorResponse:{}[{}]",
        Thread.currentThread().getStackTrace()[2].getMethodName(),
        e.getMessage());
    log.error(e.getMessage(), e);
    return e instanceof CommonException && !StringUtils.isEmpty(e.getMessage())
        ? fail(e.getMessage())
        : fail();
  }
}
