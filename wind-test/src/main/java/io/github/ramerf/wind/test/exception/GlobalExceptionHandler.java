package io.github.ramerf.wind.test.exception;

import com.alibaba.fastjson.JSONException;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.StringUtils;
import java.sql.SQLException;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.mybatis.spring.MyBatisSystemException;
import org.springframework.core.convert.ConversionFailedException;
import org.springframework.dao.DataAccessException;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

/**
 * The type Global exception handler.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/5
 */
@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {

  @ExceptionHandler(value = Exception.class)
  @ResponseBody
  @ResponseStatus
  public ResponseEntity<Rs<Object>> handle(HttpServletRequest request, Exception exception) {
    log.error(request.getRequestURL().toString());
    // 通用异常
    if (exception instanceof CommonException) {
      handleError(request, exception);
      final ResultCode resultCode = ((CommonException) exception).getResultCode();
      return Objects.isNull(resultCode) ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
    } else if (exception instanceof MethodArgumentTypeMismatchException) {
      // 参数为枚举类型时,枚举值无效.
      if (exception.getCause() instanceof ConversionFailedException
          && exception.getCause().getCause() instanceof CommonException) {
        final CommonException commonException = (CommonException) exception.getCause().getCause();
        log.warn(commonException.getMessage(), commonException);
        final ResultCode resultCode = commonException.getResultCode();
        return Objects.isNull(resultCode) ? Rs.fail(exception.getMessage()) : Rs.fail(resultCode);
      }
      // 方法参数格式不匹配
      handleError(request, exception);
      final String fieldName = ((MethodArgumentTypeMismatchException) exception).getName();
      return Rs.wrongFormat(fieldName);
    } else if (exception instanceof MissingServletRequestParameterException) {
      // 方法参数未传递
      handleError(request, exception);
      final String fieldName =
          ((MissingServletRequestParameterException) exception).getParameterName();
      return Rs.notPresent(fieldName);
    } else if (exception instanceof HttpMessageNotReadableException) {
      // 请求体为空或请求体解析失败
      if (exception.getCause() instanceof JSONException) {
        handleError(request, (JSONException) exception.getCause());
        return Rs.wrongFormat("json");
      }
      handleError(request, exception);
      return Rs.notPresent("请求体");
    } else if (exception instanceof HttpMediaTypeNotSupportedException) {
      // 请求Content-type不支持
      handleError(request, exception);
      return Rs.notSupportContentType(request.getContentType());
    } else if (exception instanceof HttpRequestMethodNotSupportedException) {
      // 请求方式不支持
      handleError(request, exception);
      return Rs.notSupportMethod(request.getMethod());
    } else if (exception instanceof MyBatisSystemException) {
      final Throwable cause = exception.getCause();
      if (cause instanceof TooManyResultsException) {
        // 返回结果过多
        handleError(request, exception);
        return Rs.tooManyResults(request.getMethod());
      }
    } else if (exception instanceof ConstraintViolationException) {
      return Rs.fail(
          "提交信息有误:<br/>"
              .concat(
                  ((ConstraintViolationException) exception)
                      .getConstraintViolations().stream()
                          .map(ConstraintViolation::getMessage)
                          .collect(Collectors.joining("<br/>"))));
    } else if (exception instanceof DataAccessException) {
      // 处理该异常主要便于开发快速定位问题
      final Throwable cause = exception.getCause();
      if (cause instanceof SQLException) {
        handleError(request, (SQLException) cause);
        // 只有非线上环境才会返回具体的代码错误信息,否则返回系统繁忙
        return Rs.fail(
            StringUtils.nonEmpty(cause.getMessage())
                ? ResultCode.ERROR_DATA_ACCESS.desc(cause.getMessage())
                : ResultCode.ERROR);
      }
    }
    handleError(request, exception);
    return Rs.fail();
  }

  @SuppressWarnings("unused")
  private void handleError(HttpServletRequest request, Exception exception) {
    log.error(exception.getMessage(), exception);
    /// 模板引擎用
    // request.setAttribute("error", exception.getMessage());
  }
}
