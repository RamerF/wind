package io.github.ramerf.wind.core.dialect.identity;

import io.github.ramerf.wind.core.exception.MappingException;
import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Type;

/**
 * Represents a support for the Dialect identity key generation
 *
 * @author Andrea Boriero
 * @since 5.1
 */
public interface IdentityColumnSupport {

  /**
   * {@link #getIdentityColumnString(Type)}是否包含数据类型.
   *
   * @return boolean
   */
  boolean containDataTypeInIdentityColumn();

  /**
   * The syntax used during DDL to define a column as being an IDENTITY of a particular type.
   *
   * @param type The type.
   * @return The appropriate DDL fragment.
   * @throws MappingException If IDENTITY generation is not supported.
   */
  String getIdentityColumnString(Type type) throws WindException;
}
