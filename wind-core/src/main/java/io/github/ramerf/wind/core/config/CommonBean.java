package io.github.ramerf.wind.core.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/29
 */
@Slf4j
@Configuration("wind_core_common_bean")
public class CommonBean {

  //  @Order(5)
  //  @Autowired
  //  public void initRedisCacheRedisTemplate(final RedisTemplate<String, Object> redisTemplate) {
  // 后期需要加上redis缓存
  //    RedisCache.setRedisTemplate(redisTemplate);
  //  }
}
