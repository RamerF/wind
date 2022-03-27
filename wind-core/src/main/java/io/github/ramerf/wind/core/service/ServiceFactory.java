package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.plugin.TransactionalAdaptor;
import java.io.Serializable;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
public class ServiceFactory {
  public static <S extends BaseService<T, ID>, T, ID extends Serializable> S getService(
      @Nonnull S service) {
    //noinspection unchecked
    return (S) TransactionalAdaptor.intercept(service);
  }
}
