package io.github.ramerf.wind.core.itercepter;

import javax.annotation.Nonnull;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

/**
 * 拦截频繁请求.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/27
 */
@Slf4j
@Component
public class FrequencyRequestInterceptor extends HandlerInterceptorAdapter {
  @Override
  public boolean preHandle(
      @Nonnull final HttpServletRequest request,
      @Nonnull final HttpServletResponse response,
      @Nonnull final Object handler)
      throws Exception {
    log.info(
        "preHandle:[method:{},uri:{},handler:{}]",
        request.getMethod(),
        request.getRequestURI(),
        handler);
    return super.preHandle(request, response, handler);
  }
}
