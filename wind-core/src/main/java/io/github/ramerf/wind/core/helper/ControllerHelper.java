package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.util.CollectionUtils;
import io.github.ramerf.wind.core.util.PageUtils;
import java.lang.reflect.Field;
import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.validation.*;

import static io.github.ramerf.wind.core.entity.response.Rs.*;
import static io.github.ramerf.wind.core.util.BeanUtils.getNullProp;
import static io.github.ramerf.wind.core.util.BeanUtils.initial;
import static io.github.ramerf.wind.core.util.EntityUtils.getPoJoClass;

/**
 * 简化Controller操作.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@SuppressWarnings({"unused", "rawtypes", "unchecked"})
public final class ControllerHelper {
  /**
   * 执行创建.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param error 执行失败时的错误码,可以为null
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R> void create(
      final S invoke, final T entity, final ResultCode error) {
    try {
      invoke.create(entity);
      if (Objects.isNull(entity.getId())) {
        throw CommonException.of(ResultCode.API_FAIL_EXEC_CREATE);
      }
    } catch (Exception e) {
      throw CommonException.of(Objects.nonNull(error) ? error : ResultCode.API_FAIL_EXEC_CREATE);
    }
  }

  /**
   * Create response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param bindingResult the binding result
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<R>> create(
          final S invoke, final T entity, final BindingResult bindingResult) {
    log.info("create:[{}]", entity);
    if (bindingResult.hasErrors()) {
      return fail(collectBindingResult(bindingResult));
    }
    try {
      return ok(json().put("id", invoke.create(entity)), ResultCode.API_SUCCESS_EXEC_CREATE.desc());
    } catch (Exception e) {
      return errorResponse(e);
    }
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
   * @param includeNullProps 即使值为null也保存的属性
   * @return the response entity
   */
  public static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> create(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final IFunction<T, ?>... includeNullProps) {
    return createOrUpdate(invoke, entity, bindingResult, true, includeNullProps);
  }

  /**
   * 通过Id获取实体详情.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the service
   * @param id the id
   * @return the entity
   * @see #detail(BaseService, long, Function) #detail(BaseService, long, Function)
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<R>> detail(final S invoke, final long id) {
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
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<R>> detail(final S invoke, final long id, Function<T, R> function) {
    if (id < 1) {
      return wrongFormat("id");
    }
    final T t = invoke.getById(id);
    return Objects.nonNull(function) ? ok(function.apply(t)) : ok(t);
  }

  /**
   * 转换分页对象.将Page domain对象转换为 Page 任意对象,并封装为页面响应对象.
   *
   * @param <T> domain对象
   * @param <R> the type parameter
   * @param page {@link Page} domain对象
   * @param function 转换函数表达式
   * @return {@link ResponseEntity}
   */
  public static <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
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
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<R>> update(
          final S invoke, final T entity, final long id, final BindingResult bindingResult) {
    if (id < 1) {
      return wrongFormat("id");
    }
    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return fail(collectBindingResult(bindingResult));
    }
    entity.setId(id);
    try {
      final Optional<Integer> update = invoke.update(entity);
      return update.isPresent()
          ? notExist(String.valueOf(id))
          : ok(json().put("id", entity.getId()), ResultCode.API_SUCCESS_EXEC_UPDATE.desc());
    } catch (Exception e) {
      return errorResponse(e);
    }
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
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<Long>> update(final S invoke, final T entity) {
    return update(invoke, entity, null, null);
  }

  /**
   * 执行更新.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   * @see #update(BaseService, AbstractEntityPoJo, ResultCode, ResultCode) #update(BaseService,
   *     AbstractEntityPoJo, ResultCode, ResultCode)
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<Long>> update(final S invoke, final T entity, final ResultCode errorCode) {
    return update(invoke, entity, null, errorCode);
  }

  /**
   * 执行更新.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param successCode 执行成功时的响应码,可以为null
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<Long>> update(
          final S invoke,
          final T entity,
          final ResultCode successCode,
          final ResultCode errorCode) {
    try {
      final Optional<Integer> update = invoke.update(entity);
      return update.isPresent()
          ? fail(ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST)
          : Objects.nonNull(successCode) ? ok(entity.getId(), successCode) : ok();
    } catch (Exception e) {
      return fail(Objects.nonNull(errorCode) ? errorCode : ResultCode.API_FAIL_EXEC_UPDATE);
    }
  }

  /**
   * Update response entity.
   *
   * @param <S> the type parameter
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param <P> the type parameter
   * @param invoke the invoke
   * @param entity the entity
   * @param id the id
   * @param bindingResult the binding result
   * @param includeNullProps 即使值为null也更新的属性
   * @return the response entity
   */
  public static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> update(
          final S invoke,
          final R entity,
          final long id,
          final BindingResult bindingResult,
          final IFunction<T, ?>... includeNullProps) {
    if (id < 1) {
      return wrongFormat("id");
    }
    entity.setId(id);
    return createOrUpdate(invoke, entity, bindingResult, false, includeNullProps);
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
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<R>> delete(final S invoke, final long id) {
    if (id < 1) {
      return wrongFormat("id");
    }
    try {
      invoke.delete(id);
    } catch (Exception e) {
      return errorResponse(e);
    }
    return ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  /**
   * 执行删除,不包含返回值.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param runnable 执行删除操作
   * @param success 删除成功执行方法
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<String>> delete(
          final Runnable runnable,
          final Supplier<ResponseEntity<Rs<String>>> success,
          final ResultCode errorCode) {
    try {
      runnable.run();
      return success.get();
    } catch (Exception e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return fail(Objects.nonNull(errorCode) ? errorCode : ResultCode.API_FAIL_EXEC_DELETE);
    }
  }

  /**
   * 执行删除,包含返回值.
   *
   * @param <S> the service
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param result 执行删除操作后的结果
   * @param function 返回结果处理,如参为删除返回结果<br>
   *     注意: 批量删除时,如果返回个数与实际个数不同(即只删除了部分记录)
   * @param errorCode 执行失败时的错误码,可以为null
   * @return the response entity
   */
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo, R>
      ResponseEntity<Rs<String>> delete(
          final R result,
          final Function<R, ResponseEntity<Rs<String>>> function,
          final ResultCode errorCode) {
    try {
      return function.apply(result);
    } catch (Exception e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return fail(Objects.nonNull(errorCode) ? errorCode : ResultCode.API_FAIL_EXEC_DELETE);
    }
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
  public static <S extends BaseService<T>, T extends AbstractEntityPoJo>
      ResponseEntity<Rs<String>> deleteByIds(final S invoke, final List<Long> ids) {
    try {
      invoke.deleteByIds(ids);
    } catch (Exception e) {
      return errorResponse(e);
    }
    return ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  /**
   * 转换集合对象.将List domain对象转换为 List 任意对象,并封装为页面响应对象.
   *
   * @param <T> domain对象
   * @param <E> 任意对象(通常是response|poJo对象)
   * @param lists List domain对象
   * @param function 转换函数表达式
   * @param filterFunction 过滤函数表达式
   * @return {@link ResponseEntity}
   */
  <T extends AbstractEntity, E> ResponseEntity<Rs<List<E>>> list(
      final List<T> lists, final Function<T, E> function, final Predicate<E> filterFunction) {
    return ok(CollectionUtils.toList(lists, function, filterFunction));
  }

  /**
   * 将 {@link List} 转换为 {@link Page}.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param page the page
   * @return the response entity
   */
  public static <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(final List<T> page) {
    return ok(PageUtils.toPage(page));
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
  public static <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
      final List<T> page, final Function<T, R> function, final Predicate<R> filterFunction) {
    return ok(PageUtils.toPage(page, function, filterFunction));
  }

  /**
   * Execute response entity.
   *
   * @param <R> the type parameter
   * @param result the result
   * @return the response entity
   */
  public static <R> ResponseEntity<Rs<R>> execute(final R result) {
    return ok(result);
  }

  /**
   * Execute response entity.
   *
   * @param <R> the type parameter
   * @param result the result
   * @param bindingResult the binding result
   * @return the response entity
   */
  public static <R> ResponseEntity<Rs<R>> execute(
      final R result, final BindingResult bindingResult) {

    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return fail(collectBindingResult(bindingResult));
    }
    return ok(ResultCode.SUCCESS);
  }

  /**
   * Collect first binding result string.
   *
   * @param bindingResult the binding result
   * @return the string
   */
  public static String collectFirstBindingResult(BindingResult bindingResult) {
    ObjectError error = bindingResult.getAllErrors().get(0);
    return Objects.requireNonNull(error.getDefaultMessage()).contains("Failed to convert property")
        ? ((FieldError) error).getField() + " 格式不正确"
        : error.getDefaultMessage();
  }

  /**
   * Collect binding result string.
   *
   * @param bindingResult the binding result
   * @return the string
   */
  public static String collectBindingResult(BindingResult bindingResult) {
    StringBuilder errorMsg = new StringBuilder();
    bindingResult
        .getAllErrors()
        .forEach(
            error ->
                errorMsg
                    .append("<br/>")
                    .append(
                        Objects.requireNonNull(error.getDefaultMessage())
                                .contains("Failed to convert property")
                            ? ((FieldError) error).getField() + " 格式不正确"
                            : error.getDefaultMessage()));
    final String msg = errorMsg.toString().replaceFirst("<br/>", "");
    return msg.contains("<br/>") ? "提交信息有误: <br/>".concat(msg) : msg;
  }

  /**
   * 创建或更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的request {@link AbstractEntityRequest} 对象.
   * @param bindingResult 校验器校验结果.
   * @param create 是否是创建.
   * @param includeNullProps 即使值为null也保存的属性
   * @param <T> 服务层实现类.
   * @return {@link ResponseEntity}
   */
  private static <
          S extends BaseService<T>,
          T extends AbstractEntityPoJo,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> createOrUpdate(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final boolean create,
          @Nonnull final IFunction<T, ?>... includeNullProps) {
    final List<String> includeNulls =
        Arrays.stream(includeNullProps)
            .map(BeanFunction::getField)
            .map(Field::getName)
            .collect(Collectors.toList());
    if (log.isDebugEnabled()) {
      // 打印参数,处理不同层级调用的情况
      Stream.of(Thread.currentThread().getStackTrace())
          .filter(o -> o.getClassName().contains("controller."))
          .findFirst()
          .ifPresent(
              stack -> log.info("\n\t{}:[entity:{},includeNulls:{}]", stack, entity, includeNulls));
    }
    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return fail(collectBindingResult(bindingResult));
    }
    final Long id = entity.getId();
    // 创建对象时id置为空
    if (create) {
      entity.setId(null);
    } else if (Objects.isNull(id) || id < 1) {
      return wrongFormat("id");
    } else {
      // 更新时id对应的数据不存在
      final T exist = invoke.getById(id);
      CommonException.requireNonNull(exist, ResultCode.API_DATA_NOT_EXIST.desc(String.valueOf(id)));
    }
    T domain = initial(getPoJoClass(invoke));
    BeanUtils.copyProperties(
        entity,
        domain,
        getNullProp(entity).stream()
            .filter(prop -> !includeNulls.contains(prop))
            .toArray(String[]::new));
    // 额外处理
    entity.redundantValue(domain);
    log.debug("createOrUpdate:[{}]", domain);
    try {
      long affectRow = create ? invoke.create(domain) > 0 ? 1 : 0 : invoke.update(domain).orElse(0);
      return affectRow == 1
          ? ok(
              json().put("id", domain.getId()),
              create
                  ? ResultCode.API_SUCCESS_EXEC_CREATE.desc()
                  : ResultCode.API_SUCCESS_EXEC_UPDATE.desc())
          : fail(
              create
                  ? ResultCode.API_FAIL_EXEC_CREATE.desc()
                  : ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST.desc());
    } catch (Exception e) {
      return errorResponse(e);
    }
  }

  private static <R> ResponseEntity<Rs<R>> errorResponse(Exception e) {
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
