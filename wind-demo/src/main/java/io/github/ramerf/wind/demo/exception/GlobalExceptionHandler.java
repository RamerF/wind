package io.github.ramerf.wind.demo.exception;

import io.github.ramerf.wind.core.entity.response.Rs;
import javax.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

/**
 * 自定义异常处理.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/5
 * @see io.github.ramerf.wind.core.exception.GlobalExceptionHandler
 */
@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {
  /**
   * 自定义异常处理,会覆盖wind中相应的异常处理.
   *
   * <p><b>如果捕捉了{@link Exception},即{@code @ExceptionHandler(value =
   * Exception.class)}*,会覆盖wind中的所有异常处理定义.</b>
   *
   * @param request the request
   * @param exception the exception
   * @return rs
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = NullPointerException.class)
  public Rs<Object> handleNullPointerException(HttpServletRequest request, Exception exception) {
    log.error(exception.getMessage(), exception);
    return Rs.fail(exception.getMessage());
  }
}
