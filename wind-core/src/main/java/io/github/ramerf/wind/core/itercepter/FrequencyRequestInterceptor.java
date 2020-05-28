package io.github.ramerf.wind.core.itercepter;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Date;
import java.util.Objects;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.*;
import org.springframework.http.MediaType;
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
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Resource private WindConfiguration windConfiguration;

  @Override
  public boolean preHandle(
      @Nonnull final HttpServletRequest request,
      @Nonnull final HttpServletResponse response,
      @Nonnull final Object handler)
      throws Exception {
    final String requestId = request.getHeader("requestId");
    log.info(
        "preHandle:[requestId:{},method:{},uri:{},handler:{}]",
        requestId,
        request.getMethod(),
        request.getRequestURI(),
        handler);
    final ValueOperations<String, Object> valueOperations = redisTemplate.opsForValue();
    if (StringUtils.isEmpty(requestId)) {
      response.setContentType("application/json; charset=utf-8");
      response
          .getWriter()
          .write(
              new ObjectMapper()
                  .writeValueAsString(
                      Rs.ok(Rs.json().put("error", "header should contain requestId.")).getBody()));
      return false;
    }
    // 如果已存在,表明是频繁提交
    final String key = "requestId" + requestId;
    final Date exist = (Date) valueOperations.get(key);
    if (Objects.nonNull(exist) && exist.after(new Date())) {
      response.setContentType(MediaType.APPLICATION_JSON_UTF8_VALUE);
      response
          .getWriter()
          .write(
              new ObjectMapper()
                  .writeValueAsString(
                      Rs.ok(Rs.json().put("error", "frequency request.")).getBody()));
      return false;
    }
    valueOperations.set(
        key,
        new Date(
            System.currentTimeMillis()
                + windConfiguration.getFrequencyRequestIntercept().getTime()));
    return true;
  }
}
