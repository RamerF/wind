package io.github.ramerf.mybatisturbo.core.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.mybatisturbo.core.entity.response.AbstractEntityResponse;
import io.github.ramerf.mybatisturbo.core.entity.response.Rs;
import io.github.ramerf.mybatisturbo.core.helper.ControllerHelper;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;

/**
 * 替换为{@link ControllerHelper}.<br>
 * 示例:<br>
 * <code>ControllerHelper.create(service, entity, bindingResult)</code>
 *
 * @author Tang Xiaofeng
 * @since 2019/11/20
 */
@Deprecated
public interface CommonService {

  /**
   * 创建.
   *
   * @param invoke 服务层实现类.
   * @param entity 要保存的对象.
   * @param bindingResult 校验器校验结果.
   * @param <S> 服务层实现类service {@link BaseService}.
   * @param <T> Domain对象 {@link AbstractEntityPoJo}.
   * @param <E> PoJo对象 {@link AbstractEntityResponse}.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
      ResponseEntity<Rs<R>> create(
          final S invoke, final T entity, final BindingResult bindingResult);

  /**
   * 创建.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的request {@link AbstractEntityRequest} 对象.
   * @param bindingResult 校验器校验结果.
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest,
          P>
      ResponseEntity<Rs<P>> create(
          final S invoke,
          final R entity,
          final BindingResult bindingResult,
          final String... includeNullProperties);

  /**
   * 创建.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的request {@link AbstractEntityRequest} 对象.
   * @param bindingResult 校验器校验结果.
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <
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
          final String... includeNullProperties);

  /**
   * 根据id获取详情.
   *
   * @param invoke 服务层实现类.
   * @param id persist id.
   * @param <S> 服务层实现类service {@link BaseService}.
   * @param <T> Domain对象 {@link AbstractEntityPoJo}.
   * @param <E> PoJo对象 {@link AbstractEntityResponse}.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
      ResponseEntity<Rs<R>> detail(final S invoke, final long id, Function<T, R> function);

  /**
   * 更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的 {@link AbstractEntityPoJo} 对象.
   * @param id 页面传递的id.
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
      ResponseEntity<Rs<R>> update(
          final S invoke, final T entity, final long id, BindingResult bindingResult);

  /**
   * 更新,该方法已弃用.<br>
   * 替代方法: {@link CommonService#update(BaseService, AbstractEntityRequest, long, BindingResult,
   * String...)}
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的 request {@link AbstractEntityRequest} 对象.
   * @param id 路径上的 id
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  @Deprecated
  <
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
          final String... includeNullProperties);

  /**
   * 更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的 request {@link AbstractEntityRequest} 对象.
   * @param id 路径上的 id
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <
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
          final String... includeNullProperties);

  /**
   * 更新.
   *
   * @param invoke 服务层实现类.
   * @param entity 要更新的 request {@link AbstractEntityRequest} 对象.
   * @param id 路径上的 id
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param bindingResult 校验器校验结果.
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <
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
          final String... includeNullProperties);

  /**
   * 批量更新.
   *
   * @param invoke 服务层实现类.
   * @param includeNullProperties 默认情况下不会覆写值为null的值,如果需要覆写,这里传入
   * @param <T> 服务层实现类.
   * @param <E> 要更新的对象.
   * @return {@link ResponseEntity}
   */
  <
          S extends BaseService<T, E>,
          T extends AbstractEntityPoJo,
          E extends AbstractEntityResponse,
          R extends AbstractEntityRequest>
      ResponseEntity<Rs<List<Long>>> updateBatch(
          final S invoke, final List<R> entities, final String... includeNullProperties);

  /**
   * 逻辑删除.
   *
   * @param <T> 服务层实现类.
   * @param <E> 要删除的对象.
   * @param invoke 服务层实现类.
   * @param id 待删除对象id.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
      ResponseEntity<Rs<R>> delete(final S invoke, final long id);

  /**
   * 逻辑删除.
   *
   * @param <T> 服务层实现类.
   * @param <E> 要删除的对象.
   * @param invoke 服务层实现类.
   * @param id 待删除对象id.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
      ResponseEntity<Rs<R>> delete(final S invoke, final long id, final long companyId);

  /**
   * 逻辑删除批量.
   *
   * @param <T> 服务层实现类.
   * @param <E> 要删除的对象.
   * @param invoke 服务层实现类.
   * @param ids 待删除对象id集合.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse>
      ResponseEntity<Rs<String>> deleteBatch(final S invoke, final List<Long> ids);

  /**
   * 逻辑删除批量.
   *
   * @param <T> 服务层实现类.
   * @param <E> 要删除的对象.
   * @param invoke 服务层实现类.
   * @param ids 待删除对象id集合.
   * @return {@link ResponseEntity}
   */
  <S extends BaseService<T, E>, T extends AbstractEntityPoJo, E extends AbstractEntityResponse>
      ResponseEntity<Rs<String>> deleteBatch(
          final S invoke, final List<Long> ids, final long companyId);

  /**
   * 转换分页对象.将Page domain对象转换为 Page 任意对象,并封装为页面响应对象.<br>
   * 替换为: {@link ControllerHelper#page(Page, Function)}
   *
   * @param page {@link Page} domain对象
   * @param function 转换函数表达式
   * @param <T> domain对象
   * @return {@link ResponseEntity}
   */
  @Deprecated
  <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
      final Page<T> page, final Function<T, R> function);

  /**
   * 将 {@link List} 转换为 {@link Page}.<br>
   * 替换为: {@link ControllerHelper#page(List, Function, Predicate)}
   */
  @Deprecated
  <T extends AbstractEntity, R> ResponseEntity<Rs<Page<R>>> page(
      final List<T> page, final Function<T, R> function);

  /**
   * 拼接表单错误信息.
   *
   * @param bindingResult {@link BindingResult}
   * @return 错误提示
   */
  String collectBindingResult(final BindingResult bindingResult);
}
