package io.github.ramerf.mybatisturbo.core.exception;

import io.github.ramerf.mybatisturbo.core.entity.response.ResultCode;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 全局通用异常.
 *
 * @author Tang Xiaofeng
 * @since 2019/11/13
 */
@Slf4j
@SuppressWarnings("unused")
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
    log.error("[{}]", message);
  }

  private CommonException(@Nonnull final ResultCode resultCode) {
    super(resultCode.desc());
    this.resultCode = resultCode;
    log.error("[{}:{}]", resultCode.code(), resultCode.desc());
  }

  private CommonException() {
    super(ResultCode.ERROR.desc());
    final ResultCode error = ResultCode.ERROR;
    this.resultCode = error;
    log.error("[{}:{}]", error.code(), error.desc());
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
  /// TODO-ZRH: 可以考虑子类实现
  //  /**
  //   * 异常构造函数会调用 fillInStackTrace() 构建整个调用栈，消耗较大
  //   * 而 CommonException 都是已知问题手动抛出,可使用log.err, 无需使用调用栈信息，覆盖此方法用于提升性能
  //   */
  //  @Override
  //  public Throwable fillInStackTrace() {
  //    return this;
  //  }
}
