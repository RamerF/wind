package io.github.ramerf.mybatisturbo.core.service;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.mybatisturbo.core.entity.response.*;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;

/**
 * 通用业务实现.
 *
 * @author Tang Xiaofeng
 * @since 2019/11/20
 */
@Slf4j
@Component("coms")
public class CommonServiceImpl implements CommonService {
  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R>
      ResponseEntity<Rs<R>> create(
          final S invoke, final T entity, final BindingResult bindingResult) {
    if (bindingResult.hasErrors()) {
      return Rs.fail(collectBindingResult(bindingResult));
    }
    try {
      final T create = invoke.create(entity);
      return Objects.isNull(create)
          ? Rs.fail()
          : Rs.ok(
              new JSONObject() {
                {
                  put("id", create.getId());
                }
              },
              ResultCode.API_SUCCESS_EXEC_CREATE.desc());
    } catch (Exception e) {
      return errorResponse("create", e);
    }
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> create(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final String... includeNullProperties) {
    return createOrUpdate(invoke, entity, bindingResult, true, includeNullProperties);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> create(
          final S invoke,
          final R entity,
          final long companyId,
          final BindingResult bindingResult,
          final String... includeNullProperties) {
    return createOrUpdate(invoke, entity, bindingResult, true, companyId, includeNullProperties);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R>
      ResponseEntity<Rs<R>> detail(final S invoke, final long id, Function<T, R> function) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    return Objects.nonNull(function)
        ? Rs.ok(function.apply(invoke.getById(id)))
        : Rs.ok(invoke.getById(id));
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R>
      ResponseEntity<Rs<R>> update(
          final S invoke, final T entity, final long id, final BindingResult bindingResult) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    if (bindingResult.hasErrors()) {
      return Rs.fail(collectBindingResult(bindingResult));
    }
    entity.setId(id);
    try {
      T update = invoke.update(entity);
      return Objects.isNull(update)
          ? Rs.notExist(String.valueOf(id))
          : Rs.ok(
              new JSONObject() {
                {
                  put("id", update.getId());
                }
              },
              ResultCode.API_FAIL_EXEC_UPDATE.desc());
    } catch (Exception e) {
      return errorResponse("update", e);
    }
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> update(
          final S invoke,
          final Class<T> clazz,
          final R entity,
          final long id,
          final BindingResult bindingResult,
          final String... includeNullProperties) {
    return update(invoke, entity, id, bindingResult, includeNullProperties);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> update(
          final S invoke,
          final R entity,
          final long id,
          final BindingResult bindingResult,
          final String... includeNullProperties) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    try {
      Objects.requireNonNull(BeanUtils.findDeclaredMethod(entity.getClass(), "setId", Long.class))
          .invoke(entity, id);
    } catch (Exception e) {
      return errorResponse("update", e);
    }
    return createOrUpdate(invoke, entity, bindingResult, false, includeNullProperties);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> update(
          final S invoke,
          final R entity,
          final long id,
          final long companyId,
          final BindingResult bindingResult,
          final String... includeNullProperties) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    try {
      Objects.requireNonNull(BeanUtils.findDeclaredMethod(entity.getClass(), "setId", Long.class))
          .invoke(entity, id);
    } catch (Exception e) {
      return errorResponse("update", e);
    }
    return createOrUpdate(invoke, entity, bindingResult, false, companyId, includeNullProperties);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest>
      ResponseEntity<Rs<List<Long>>> updateBatch(
          final S invoke, final List<R> entities, final String... includeNullProperties) {
    try {
      return Rs.ok(
          invoke.updateBatch(entities, includeNullProperties).stream()
              .map(AbstractEntityPoJo::getId)
              .collect(Collectors.toList()));
    } catch (Exception e) {
      return errorResponse("updateBatch", e);
    }
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R>
      ResponseEntity<Rs<R>> delete(final S invoke, final long id) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    try {
      invoke.delete(id);
    } catch (Exception e) {
      return errorResponse("delete", e);
    }
    return Rs.ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R>
      ResponseEntity<Rs<R>> delete(final S invoke, final long id, final long companyId) {
    if (id < 1) {
      return Rs.wrongFormat("id");
    }
    try {
      invoke.delete(id, companyId);
    } catch (Exception e) {
      return errorResponse("delete", e);
    }
    return Rs.ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse>
      ResponseEntity<Rs<String>> deleteBatch(final S invoke, final List<Long> ids) {
    try {
      invoke.deleteBatch(ids);
    } catch (Exception e) {
      return errorResponse("deleteBatch", e);
    }
    return Rs.ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  @Override
  public <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse>
      ResponseEntity<Rs<String>> deleteBatch(
          final S invoke, final List<Long> ids, final long companyId) {
    try {
      invoke.deleteBatch(ids, companyId);
    } catch (Exception e) {
      return errorResponse("deleteBatch", e);
    }
    return Rs.ok(ResultCode.API_SUCCESS_EXEC_DELETE);
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
      final Page<T> page, final Function<T, R> function) {
    return Rs.ok(
        Objects.isNull(page) || CollectionUtils.isEmpty(page.getRecords())
            ? new Page<>()
            : ((Page<R>) page)
                .setRecords(page.getRecords().stream().map(function).collect(Collectors.toList())));
  }

  @Override
  public <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
      List<T> list, final Function<T, R> function) {
    Page<T> page = new Page<>();
    page.setRecords(list);
    return Rs.ok(
        Optional.ofNullable(page(page, function).getBody()).map(Rs::getData).orElse(new Page<>()));
  }

  @Override
  public String collectBindingResult(BindingResult bindingResult) {
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
   * @param create 是否是创建.
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  private <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> createOrUpdate(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final boolean create,
          final String... includeNullProperties) {
    if (Objects.nonNull(bindingResult) && bindingResult.hasErrors()) {
      return Rs.fail(collectBindingResult(bindingResult));
    }
    try {
      if (create) {
        Objects.requireNonNull(BeanUtils.findDeclaredMethod(entity.getClass(), "setId", Long.class))
            .invoke(entity, (Long) null);
      }
      long start = System.currentTimeMillis();
      final T obj = invoke.createOrUpdate(entity, includeNullProperties);
      return Objects.isNull(obj) || Objects.isNull(obj.getId())
          ? Rs.fail(
              create
                  ? ResultCode.API_FAIL_EXEC_ADD.desc()
                  : ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST.desc())
          : Rs.ok(
              new JSONObject() {
                {
                  put("id", obj.getId());
                }
              },
              create
                  ? ResultCode.API_SUCCESS_EXEC_CREATE.desc()
                  : ResultCode.API_SUCCESS_EXEC_UPDATE.desc());
    } catch (Exception e) {
      return errorResponse("createOrUpdate", e);
    }
  }

  /**
   * 创建或更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的request {@link AbstractEntityRequest} 对象.
   * @param create 是否是创建.
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  private <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> createOrUpdate(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final boolean create,
          final long companyId,
          final String... includeNullProperties) {
    if (bindingResult.hasErrors()) {
      return Rs.fail(collectBindingResult(bindingResult));
    }
    try {
      if (create) {
        Objects.requireNonNull(BeanUtils.findDeclaredMethod(entity.getClass(), "setId", Long.class))
            .invoke(entity, (Long) null);
        Objects.requireNonNull(
                BeanUtils.findDeclaredMethod(entity.getClass(), "setCompanyId", Long.class))
            .invoke(entity, companyId);
      } else if (!Objects.equals(companyId, entity.getCompanyId())) {
        // 防止更新其他企业的数据
        return Rs.forbidden();
      }
      final T obj = invoke.createOrUpdate(entity, includeNullProperties);
      return Objects.isNull(obj)
          ? Rs.fail(
              create
                  ? ResultCode.API_FAIL_EXEC_ADD.desc()
                  : ResultCode.API_FAIL_EXEC_UPDATE_NOT_EXIST.desc())
          : Rs.ok(
              new JSONObject() {
                {
                  put("id", obj.getId());
                }
              },
              create
                  ? ResultCode.API_SUCCESS_EXEC_CREATE.desc()
                  : ResultCode.API_SUCCESS_EXEC_UPDATE.desc());
    } catch (Exception e) {
      return errorResponse("createOrUpdate", e);
    }
  }

  private <R> ResponseEntity<Rs<R>> errorResponse(final String methodName, Exception e) {
    log.warn("{}:[{}]", methodName, e.getMessage());
    log.error(e.getMessage(), e);
    return e instanceof CommonException && !StringUtils.isEmpty(e.getMessage())
        ? Rs.fail(e.getMessage())
        : Rs.fail();
  }
}
