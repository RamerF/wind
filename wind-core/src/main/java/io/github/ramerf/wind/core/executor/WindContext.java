package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.Configuration;
import lombok.Data;

/**
 * wind上下文.包含配置信息,管理的entity信息.
 *
 * @since 2022.03.05
 * @author ramer
 */
@Data
public class WindContext {
  WindContext() {}

  private Configuration configuration;
}
