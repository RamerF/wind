package io.github.ramerf.wind.core.event;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * @author ramer
 * @since 13/08/2020
 */
@Slf4j
@Component
public class InitEventListener implements ApplicationListener<InitEvent> {

  @Override
  public void onApplicationEvent(final InitEvent event) {
    log.info("InitEventListener:-----------------[{},{}]", event.getSource(), event.getData());
  }
}
