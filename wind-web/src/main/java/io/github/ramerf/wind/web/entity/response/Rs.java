package io.github.ramerf.wind.web.entity.response;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import java.lang.reflect.Type;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

/**
 * 通用JSON响应.
 *
 * @param <T> the type parameter
 * @since 2020.12.22
 * @author ramer
 */
@Data
@ApiModel("通用响应")
public class Rs<T> implements Serializable {

  /** {@link ResultCode#code()} */
  @ApiModelProperty(value = "请求结果CODE,0表示成功", example = "0")
  private int code = ResultCode.SUCCESS.code();

  /** 请求成功时,该值是具体返回内容,否则为空 */
  @ApiModelProperty(value = "请求成功时,该值是具体返回内容,否则为空", example = "{name: '名称',value: '值'}")
  private T data;

  /** {@link ResultCode#desc()} */
  @ApiModelProperty(value = "描述", example = "操作成功")
  private String msg = ResultCode.SUCCESS.desc();

  private Rs() {}

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @return the rs
   */
  public static <T> Rs<T> of() {
    return new Rs<>();
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param data the data
   * @return the rs
   */
  public static <T> Rs<T> of(final T data) {
    Rs<T> rs = new Rs<>();
    rs.setData(data);
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param msg the msg
   * @return the rs
   */
  public static <T> Rs<T> of(final String msg) {
    Rs<T> rs = new Rs<>();
    rs.setMsg(msg);
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param data the data
   * @param msg the msg
   * @return the rs
   */
  public static <T> Rs<T> of(final T data, final String msg) {
    Rs<T> rs = new Rs<>();
    rs.setData(data);
    rs.setMsg(msg);
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param code the code
   * @param data the data
   * @return the rs
   */
  public static <T> Rs<T> of(final int code, final T data) {
    Rs<T> rs = new Rs<>();
    rs.setCode(code);
    rs.setData(data);
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param resultCode the result code
   * @return the rs
   */
  public static <T> Rs<T> of(@Nonnull final ResultCode resultCode) {
    Rs<T> rs = new Rs<>();
    rs.setCode(resultCode.code());
    rs.setMsg(resultCode.desc());
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param data the data
   * @param resultCode the result code
   * @return the rs
   */
  public static <T> Rs<T> of(final T data, final ResultCode resultCode) {
    Rs<T> rs = new Rs<>();
    rs.setData(data);
    rs.setMsg(resultCode.desc());
    rs.setCode(resultCode.code());
    return rs;
  }

  /**
   * Of rs.
   *
   * @param <T> the type parameter
   * @param code the code
   * @param data the data
   * @param msg the msg
   * @return the rs
   */
  public static <T> Rs<T> of(final int code, final T data, final String msg) {
    Rs<T> rs = new Rs<>();
    rs.setCode(code);
    rs.setData(data);
    rs.setMsg(msg);
    return rs;
  }

  /**
   * 默认执行成功构造器.
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> Rs<T> ok() {
    return Rs.of();
  }

  /**
   * Ok response entity.
   *
   * @param <T> the type parameter
   * @param data the data
   * @return the response entity
   */
  public static <T> Rs<T> ok(final T data) {
    return Rs.of(data);
  }

  /**
   * Ok response entity.
   *
   * @param <T> the type parameter
   * @param data the data
   * @param msg the msg
   * @return the response entity
   */
  public static <T> Rs<T> ok(final T data, final String msg) {
    return Rs.of(data, msg);
  }

  /**
   * Ok response entity.
   *
   * @param <T> the type parameter
   * @param resultCode the result code
   * @return the response entity
   */
  public static <T> Rs<T> ok(final ResultCode resultCode) {
    return Rs.of(resultCode);
  }

  /**
   * Ok response entity.
   *
   * @param <T> the type parameter
   * @param data the data
   * @param resultCode the result code
   * @return the response entity
   */
  public static <T> Rs<T> ok(final T data, final ResultCode resultCode) {
    return Rs.of(data, resultCode);
  }

  /**
   * 成功返回空的{@link Page}对象.
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> Rs<PageImpl<T>> emptyPage() {
    return Rs.of(new PageImpl<>(Collections.emptyList()));
  }

  /**
   * 成功返回空的l{@link List}对象.
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> Rs<List<T>> emptyList() {
    return Rs.of(Collections.emptyList());
  }

  /**
   * 默认执行失败构造器.
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> Rs<T> fail() {
    return Rs.of(ResultCode.ERROR);
  }

  /**
   * Fail response entity.
   *
   * @param <T> the type parameter
   * @param msg the msg
   * @return the response entity
   */
  public static <T> Rs<T> fail(final String msg) {
    return Rs.of(ResultCode.of(ResultCode.ERROR.code(), msg));
  }

  /**
   * Fail response entity.
   *
   * @param <T> the type parameter
   * @param data the data
   * @param msg the msg
   * @return the response entity
   */
  public static <T> Rs<T> fail(final T data, final String msg) {
    return Rs.of(data, msg);
  }

  /**
   * Fail response entity.
   *
   * @param <T> the type parameter
   * @param resultCode the result code
   * @return the response entity
   */
  public static <T> Rs<T> fail(final ResultCode resultCode) {
    return Rs.of(resultCode);
  }

  /**
   * 参数格式不正确.
   *
   * @param <T> the type parameter
   * @param field the field
   * @return the response entity
   */
  public static <T> Rs<T> wrongFormat(final String field) {
    return Rs.of(ResultCode.API_PARAM_WRONG_FORMAT.desc(field));
  }

  /**
   * 参数值不正确.
   *
   * @param <T> the type parameter
   * @param field the field
   * @return the response entity
   */
  public static <T> Rs<T> wrongValue(final String field) {
    return Rs.of(ResultCode.API_PARAM_WRONG_VALUE.desc(field));
  }

  /**
   * 参数值不能为空
   *
   * @param <T> the type parameter
   * @param field the field
   * @return the response entity
   */
  public static <T> Rs<T> canNotBlank(final String field) {
    return Rs.of(ResultCode.API_PARAM_CAN_NOT_BLANK.desc(field));
  }

  /**
   * 参数值未传递
   *
   * @param <T> the type parameter
   * @param field the field
   * @return the response entity
   */
  public static <T> Rs<T> notPresent(final String field) {
    return Rs.of(ResultCode.API_PARAM_NOT_PRESENT.desc(field));
  }

  /**
   * 无访问权限,拒绝访问.
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> ResponseEntity<Rs<T>> forbidden() {
    return ResponseEntity.status(HttpStatus.FORBIDDEN).body(Rs.of(ResultCode.API_FORBIDDEN));
  }

  /**
   * 凭证过期.
   *
   * @return the response entity
   */
  public static ResponseEntity<Rs<Object>> secretExpire() {
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(Rs.of(ResultCode.API_CERT_SECRET_EXPIRE));
  }

  /**
   * 凭证为空.
   *
   * @return the response entity
   */
  public static ResponseEntity<Rs<Object>> secretEmpty() {
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(Rs.of(ResultCode.API_CERT_SECRET_EMPTY));
  }

  /**
   * 未认证.
   *
   * @return the response entity
   */
  public static ResponseEntity<Rs<Object>> unauthorized() {
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Rs.of(ResultCode.API_UNAUTHORIZED));
  }

  /**
   * 操作不允许.
   *
   * @param msg the msg
   * @return the response entity
   */
  public static ResponseEntity<Rs<Object>> notAllowed(final String msg) {
    return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
        .body(Rs.of(ResultCode.API_NOT_ALLOWED.desc(msg)));
  }

  /**
   * 资源不可用
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> ResponseEntity<Rs<T>> notFound() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Rs.of(ResultCode.API_NOT_FOUND));
  }

  /**
   * 请求Content-type不支持
   *
   * @param <T> the type parameter
   * @param contentType the content type
   * @return the response entity
   */
  public static <T> ResponseEntity<Rs<T>> notSupportMediaType(final String contentType) {
    return ResponseEntity.status(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
        .body(Rs.of(ResultCode.API_CONTENT_TYPE_NOT_SUPPORT.desc(contentType)));
  }

  /**
   * 请求方式不支持
   *
   * @param <T> the type parameter
   * @param method the method
   * @return the response entity
   */
  public static <T> ResponseEntity<Rs<T>> notSupportRequestMethod(final String method) {
    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(Rs.of(ResultCode.API_METHOD_NOT_SUPPORT.desc(method)));
  }

  /**
   * 返回结果过多
   *
   * @param <T> the type parameter
   * @return the response entity
   */
  public static <T> Rs<T> tooManyResults() {
    return Rs.of(ResultCode.API_TOO_MANY_RESULTS);
  }

  /**
   * 参数值无效.
   *
   * @param <T> the type parameter
   * @param field the field
   * @return the response entity
   */
  public static <T> Rs<T> invalid(final String field) {
    return Rs.of(ResultCode.API_PARAM_INVALID.desc(field));
  }

  /**
   * 数据已存在.
   *
   * @param <T> the type parameter
   * @param data the data
   * @return the response entity
   */
  public static <T> Rs<T> exist(final String data) {
    return Rs.of(ResultCode.API_DATA_EXIST.desc(data));
  }

  /**
   * 数据不存在.
   *
   * @param <T> the type parameter
   * @param data the data
   * @return the response entity
   */
  public static <T> Rs<T> notExist(final String data) {
    return Rs.of(ResultCode.API_DATA_NOT_EXIST.desc(data));
  }

  /**
   * Json json instance.
   *
   * @return the json instance
   */
  public static JsonInstance json() {
    return new JsonInstance();
  }

  /** The type Json instance. */
  public static class JsonInstance extends HashMap<String, Object> implements Serializable {
    @Override
    public @Nonnull JsonInstance put(final String key, final Object val) {
      super.put(key, val);
      return this;
    }
  }

  /** The type Json instance serializer. */
  @Slf4j
  public static class JsonInstanceSerializer implements ObjectSerializer {
    @Override
    public void write(
        JSONSerializer serializer, Object object, Object fieldName, Type fieldType, int features) {
      SerializeWriter serializeWriter = serializer.out;
      log.debug("write:[{}]", object.toString());
      serializeWriter.write(JSON.toJSONString(object));
    }
  }
}
