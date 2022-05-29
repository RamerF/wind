package io.github.ramerf.wind.core.io;

import javax.annotation.Nullable;

/**
 * @author ramer
 * @since 2022.05.29
 */
public interface ResourceLoader {
  String CLASSPATH_URL_PREFIX = ResourceResolver.CLASSPATH_URL_PREFIX;

  @Nullable
  ClassLoader getClassLoader();

  Resource getResource(String location);
}
