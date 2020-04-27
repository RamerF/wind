package io.github.ramerf.mybatisturbo.core.config;

import io.github.ramerf.mybatisturbo.core.conditions.Query;
import io.github.ramerf.mybatisturbo.core.conditions.Update;
import io.github.ramerf.mybatisturbo.core.support.handler.manager.HandlerManager;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Lookup;
import org.springframework.stereotype.Component;

import java.util.List;

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

  @Lookup
  @SuppressWarnings({"rawtypes"})
  public List<HandlerManager> handlerManager() {
    return null;
  }
}
