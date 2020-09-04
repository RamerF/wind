package io.github.ramerf.wind.core.exception;

import com.alibaba.fastjson.JSONException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.util.StringUtils;
import java.sql.SQLException;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.ConversionFailedException;
import org.springframework.dao.DataAccessException;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.stereotype.Component;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

/**
 * 全局异常处理.
 *
 * <p>自定义异常处理会覆盖该类中相应的异常处理.
 *
 * <p><b>注意:如果捕捉了{@link Exception},即:
 *
 * <p>{@code @ExceptionHandler(value = Exception.class)},
 *
 * <p>会覆盖该类中的所有异常处理.</b>
 *
 * @since 2020.09.04
 * @author Tang Xiaofeng
 */
@Slf4j
@ControllerAdvice
@Component("wind_global_exception_handler")
public class GlobalExceptionHandler {
  /**
   * 通用异常.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = CommonException.class)
  public ResponseEntity<Rs<Object>> handleCommonException(
      HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString());
    handleError(request, exception);
    final ResultCode resultCode = ((CommonException) exception).getResultCode();
    return Objects.isNull(resultCode) ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
  }

  /**
   * 请求体为空或请求体解析失败.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = HttpMessageNotReadableException.class)
  public ResponseEntity<Rs<Object>> handleHttpMessageNotReadableException(
      HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString());
    final Throwable cause = exception.getCause();
    if (cause instanceof JSONException) {
      handleError(request, (JSONException) cause);
      return Rs.wrongFormat("json");
    }
    if (cause instanceof JsonMappingException) {
      if (cause instanceof InvalidFormatException) {
        return Rs.invalid(((InvalidFormatException) cause).getPath().get(0).getFieldName());
      }
      return Rs.fail((cause.getCause()).getMessage());
    }
    handleError(request, exception);
    return Rs.notPresent("请求体");
  }

  /**
   * 方法参数格式不匹配.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = MethodArgumentTypeMismatchException.class)
  public ResponseEntity<Rs<Object>> handleMethodArgumentTypeMismatchException(
      HttpServletRequest request, Exception exception) {
    if (exception.getCause() instanceof ConversionFailedException
        && exception.getCause().getCause() instanceof CommonException) {
      final CommonException commonException =
          Optional.ofNullable(exception.getCause())
              .map(Throwable::getCause)
              .map(CommonException.class::cast)
              .orElse(CommonException.of(ResultCode.ERROR));
      log.warn(commonException.getMessage(), commonException);
      final ResultCode resultCode = commonException.getResultCode();
      return Objects.isNull(resultCode) ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
    }
    handleError(request, exception);
    final String fieldName = ((MethodArgumentTypeMismatchException) exception).getName();
    return Rs.wrongFormat(fieldName);
  }

  /**
   * 方法参数未传递.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = MissingServletRequestParameterException.class)
  public ResponseEntity<Rs<Object>> handleMissingServletRequestParameterException(
      HttpServletRequest request, Exception exception) {
    handleError(request, exception);
    return Rs.notPresent(((MissingServletRequestParameterException) exception).getParameterName());
  }

  /**
   * 请求Content-type不支持.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = HttpMediaTypeNotSupportedException.class)
  public ResponseEntity<Rs<Object>> handleHttpMediaTypeNotSupportedException(
      HttpServletRequest request, Exception exception) {
    handleError(request, exception);
    return Rs.notSupportContentType(request.getContentType());
  }

  /**
   * 请求方式不支持.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = HttpRequestMethodNotSupportedException.class)
  public ResponseEntity<Rs<Object>> handleHttpRequestMethodNotSupportedException(
      HttpServletRequest request, Exception exception) {
    handleError(request, exception);
    return Rs.notSupportMethod(request.getMethod());
  }

  /**
   * 参数校验失败.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = ConstraintViolationException.class)
  public ResponseEntity<Rs<Object>> handleConstraintViolationException(
      HttpServletRequest request, Exception exception) {
    return Rs.fail(
        ((ConstraintViolationException) exception)
            .getConstraintViolations().stream()
                .map(ConstraintViolation::getMessage)
                .collect(Collectors.joining("<br/>")));
  }

  /**
   * 数据访问异常.处理该异常主要便于开发快速定位问题
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = DataAccessException.class)
  public ResponseEntity<Rs<Object>> handleDataAccessException(
      HttpServletRequest request, Exception exception) {
    final Throwable cause = exception.getCause();
    if (cause instanceof SQLException) {
      handleError(request, (SQLException) cause);
      return Rs.fail(
          StringUtils.nonEmpty(cause.getMessage())
              ? ResultCode.ERROR_DATA_ACCESS.desc(cause.getMessage())
              : ResultCode.ERROR);
    }
    return Rs.fail();
  }

  /**
   * 其它异常.
   *
   * @param request the request
   * @param exception the exception
   * @return the response entity
   */
  @ResponseBody
  @ResponseStatus
  @ExceptionHandler(value = Exception.class)
  public ResponseEntity<Rs<Object>> handleException(
      HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString());
    handleError(request, exception);
    return Rs.fail();
  }

  private void handleError(HttpServletRequest request, Exception exception) {
    if (exception == null) {
      return;
    }
    log.error(exception.getMessage(), exception);
    /// 模板引擎用
    // request.setAttribute("error", exception.getMessage());
  }
}
