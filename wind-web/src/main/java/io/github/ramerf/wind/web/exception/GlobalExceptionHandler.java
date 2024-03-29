package io.github.ramerf.wind.web.exception;

import com.alibaba.fastjson.JSONException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import io.github.ramerf.wind.core.util.StringUtils;
import io.github.ramerf.wind.web.entity.response.ResultCode;
import io.github.ramerf.wind.web.entity.response.Rs;
import java.sql.SQLException;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.ConversionFailedException;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
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
 * @author ramer
 */
@Slf4j
@ControllerAdvice
@Component("wind-web-global-exception-handler")
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
  public Rs<Object> handleCommonException(HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString());
    handleError(request, exception);
    final ResultCode resultCode = ((CommonException) exception).getResultCode();
    return resultCode == null ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
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
  public Rs<Object> handleHttpMessageNotReadableException(
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
  public Rs<Object> handleMethodArgumentTypeMismatchException(
      HttpServletRequest request, Exception exception) {
    if (exception.getCause() instanceof ConversionFailedException
        && exception.getCause().getCause() instanceof CommonException) {
      final CommonException commonException =
          Optional.ofNullable(exception.getCause())
              .map(Throwable::getCause)
              .map(CommonException.class::cast)
              .orElse(new CommonException(ResultCode.ERROR));
      log.warn(commonException.getMessage(), commonException);
      final ResultCode resultCode = commonException.getResultCode();
      return resultCode == null ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
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
  public Rs<Object> handleMissingServletRequestParameterException(
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
    return Rs.notSupportMediaType(request.getContentType());
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
    return Rs.notSupportRequestMethod(request.getMethod());
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
  public Rs<Object> handleConstraintViolationException(
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
  public Rs<Object> handleDataAccessException(HttpServletRequest request, Exception exception) {
    if (exception instanceof IncorrectResultSizeDataAccessException) {
      handleError(request, exception);
      return Rs.tooManyResults();
    }
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
  public Rs<Object> handleException(HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString(), exception);
    handleError(request, exception);
    return Rs.fail();
  }

  private void handleError(HttpServletRequest request, Exception exception) {
    if (exception == null) {
      return;
    }
    log.error(exception.getMessage(), exception);
  }
}
