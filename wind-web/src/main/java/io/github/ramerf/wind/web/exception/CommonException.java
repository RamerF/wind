package io.github.ramerf.wind.web.exception;

import io.github.ramerf.wind.web.entity.response.ResultCode;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 全局通用异常.
 *
 * @since 2020.12.22
 * @author ramer
 */
@Slf4j
public class CommonException extends RuntimeException {
  private ResultCode resultCode;

  public ResultCode getResultCode() {
    return resultCode;
  }

  public static CommonException of() {
    return new CommonException();
  }

  public static CommonException of(final String message) {
    return new CommonException(message);
  }

  public static CommonException of(final int code, final String message) {
    return new CommonException(ResultCode.of(code, message));
  }

  public static CommonException of(final int code, final String message, final Throwable cause) {
    return new CommonException(ResultCode.of(code, message), cause);
  }

  public static CommonException of(@Nonnull final ResultCode resultCode) {
    return new CommonException(resultCode);
  }

  public static CommonException of(@Nonnull final ResultCode resultCode, final Throwable cause) {
    return new CommonException(resultCode, cause);
  }

  public static CommonException of(final String message, final Throwable cause) {
    return new CommonException(message, cause);
  }

  public static CommonException of(final Throwable cause) {
    return new CommonException(cause);
  }

  /** 如果对象为空,抛出异常. */
  public static Object requireNonNull(Object obj, String message) {
    if (obj == null) {
      throw CommonException.of(message);
    }
    return obj;
  }

  /** 如果对象为空,抛出异常. */
  public static Object requireNonNull(Object obj, final ResultCode resultcode) {
    if (obj == null) {
      throw CommonException.of(resultcode);
    }
    return obj;
  }

  private CommonException() {
    super(ResultCode.ERROR.desc());
    this.resultCode = ResultCode.ERROR;
  }

  private CommonException(final String message) {
    super(message);
  }

  private CommonException(@Nonnull final ResultCode resultCode) {
    super(resultCode.desc());
    this.resultCode = resultCode;
  }

  private CommonException(final String message, final Throwable cause) {
    super(message, cause);
  }

  private CommonException(@Nonnull final ResultCode resultCode, final Throwable cause) {
    super(resultCode.desc(), cause);
    this.resultCode = resultCode;
  }

  private CommonException(final Throwable cause) {
    super(cause);
  }
}
