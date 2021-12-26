package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.event.InitFinishEvent;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import lombok.Data;

/**
 * wind上下文.包含配置信息,管理的entity信息,wind初始化完成后会发布{@link InitFinishEvent}事件,可以通过监听事件获得该对象.如:
 * <pre>
 * {@code
 *
 *  @Slf4j
 *  @Component
 *  public class InitFinishEventListener implements ApplicationListener<InitFinishEvent> {
 *    @Override
 *    public void onApplicationEvent(final InitFinishEvent event) {
 *      log.info("InitFinishEventListener:[source:{},data:{}]", event.getSource(), event.getData()); }
 *    }
 *  }
 *
 * @author ramer
 * @since 22/08/2020
 * @see InitFinishEvent
 */
@Data
public class WindContext {
  WindContext() {}

  private DbMetaData dbMetaData;

  private Configuration configuration;

  private Executor executor;
}
