package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import org.springframework.cglib.proxy.LazyLoader;

/**
 * @author ramer
 * @since 26/09/2020
 */
public abstract class AbstractLazyLoader implements LazyLoader {
  final AbstractEntityPoJo poJo;
  final MappingInfo mappingInfo;
  final Object relationValue;

  public AbstractLazyLoader(final OneToOneFetch fetch) {
    this.poJo = fetch.poJo;
    this.mappingInfo = fetch.mappingInfo;
    this.relationValue = fetch.relationValue;
  }
}
