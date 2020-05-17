package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.condition.Query;
import io.github.ramerf.wind.core.condition.Update;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Lookup;
import org.springframework.stereotype.Component;

/**
 * 该类用于管理prototype类型的bean,由于违背Ioc,不建议使用{@link AppContextInject}获取bean.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/29
 */
@Slf4j
@Component
public class PrototypeBean {
  @Lookup
  public Query query() {
    return null;
  }

  @Lookup
  public Update update() {
    return null;
  }
}
