package io.github.ramerf.wind.core.event;

import io.github.ramerf.wind.core.config.WindContext;
import lombok.extern.slf4j.Slf4j;

/**
 * TODO WARN 初始化完成后会发布该事件.监听代码示例:
 *
 * <pre>{@code
 * @Slf4j
 * @Component
 * public class InitFinishEventListener implements ApplicationListener<InitFinishEvent> {
 *    @Override
 *    public void onApplicationEvent(final InitFinishEvent event) {
 *      final WindContext context = event.getWindContext();
 *      log.debug("InitFinishEventListener:[{}]", context);
 *    }
 * }
 * }</pre>
 *
 * @since 13 /08/2020
 * @author ramer
 */
@Slf4j
public class InitFinishEvent {
  private final WindContext windContext;

  /**
   * Gets wind context.
   *
   * @return the wind context
   */
  public WindContext getWindContext() {
    return windContext;
  }

  /**
   * Instantiates a new Init finish event.
   *
   * @param windContext the wind context
   */
  public InitFinishEvent(final WindContext windContext) {
    this.windContext = windContext;
    log.info("Publish init finish event.");
  }
}
