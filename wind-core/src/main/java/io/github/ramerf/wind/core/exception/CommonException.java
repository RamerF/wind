package io.github.ramerf.wind.core.exception;

import io.github.ramerf.wind.core.entity.response.ResultCode;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 全局通用异常.
 *
 * @author Tang Xiaofeng
 * @since 2019/11/13
 */
@Slf4j
@SuppressWarnings({"unused", "UnusedReturnValue"})
public class CommonException extends RuntimeException {
  private ResultCode resultCode;

  public ResultCode getResultCode() {
    return resultCode;
  }

  public static CommonException of(final String message) {
    return new CommonException(message);
  }

  public static CommonException of(final String code, final String message) {
    return new CommonException(ResultCode.of(code, message));
  }

  public static CommonException of(@Nonnull final ResultCode resultCode) {
    return new CommonException(resultCode);
  }

  public static CommonException of() {
    return new CommonException();
  }

  public static CommonException of(final String message, final Throwable cause) {
    return new CommonException(message, cause);
  }

  public static CommonException of(final Throwable cause) {
    return new CommonException(cause);
  }

  public static <T> T requireNonNull(T obj, String message) {
    if (obj == null) {
      throw CommonException.of(message);
    }
    return obj;
  }

  public static <T> T requireNonNull(T obj, final ResultCode resultcode) {
    if (obj == null) {
      throw CommonException.of(resultcode);
    }
    return obj;
  }

  private CommonException(final String message) {
    super(message);
    log.error("CommonException:[{}]", message);
  }

  private CommonException(@Nonnull final ResultCode resultCode) {
    super(resultCode.desc());
    this.resultCode = resultCode;
    log.error("CommonException:[code:{},desc:{}]", resultCode.code(), resultCode.desc());
  }

  private CommonException() {
    super(ResultCode.ERROR.desc());
    final ResultCode error = ResultCode.ERROR;
    this.resultCode = error;
    log.error("CommonException:[code:{},desc:{}]", error.code(), error.desc());
  }

  private CommonException(final String message, final Throwable cause) {
    super(message, cause);
    log.error(message, cause);
  }

  private CommonException(final Throwable cause) {
    super(cause);
    log.error(cause.getMessage(), cause);
  }

  protected CommonException(
      final String message,
      final Throwable cause,
      final boolean enableSuppression,
      final boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
    log.error(cause.getMessage(), cause);
  }
}
