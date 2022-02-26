package io.github.ramerf.wind.core.dialect.identity;

import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Type;

/** @author Andrea Boriero */
public class IdentityColumnSupportImpl implements IdentityColumnSupport {

  @Override
  public boolean containDataTypeInIdentityColumn() {
    return false;
  }

  @Override
  public String getIdentityColumnString(Type type) throws WindException {
    throw new WindException(getClass().getName() + " does not support identity key generation");
  }
}
