package io.github.ramerf.wind.core.dialect.identity;

import io.github.ramerf.wind.core.exception.CommonException;
import java.lang.reflect.Type;

/** @author Andrea Boriero */
public class IdentityColumnSupportImpl implements IdentityColumnSupport {

  @Override
  public String getIdentityColumnString(Type type) throws CommonException {
    throw CommonException.of(getClass().getName() + " does not support identity key generation");
  }
}
