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

  /** 如果对象为空,抛出异常. */
  public static Object requireNonNull(Object obj, String message) {
    if (obj == null) {
      throw new CommonException(message);
    }
    return obj;
  }

  /** 如果对象为空,抛出异常. */
  public static Object requireNonNull(Object obj, final ResultCode resultcode) {
    if (obj == null) {
      throw new CommonException(resultcode);
    }
    return obj;
  }

  public CommonException() {
    super(ResultCode.ERROR.desc());
    this.resultCode = ResultCode.ERROR;
  }

  public CommonException(final String message) {
    super(message);
  }

  public CommonException(@Nonnull final ResultCode resultCode) {
    super(resultCode.desc());
    this.resultCode = resultCode;
  }

  public CommonException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public CommonException(@Nonnull final ResultCode resultCode, final Throwable cause) {
    super(resultCode.desc(), cause);
    this.resultCode = resultCode;
  }

  public CommonException(final Throwable cause) {
    super(cause);
  }
}
