/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.identity;

import java.lang.reflect.Type;

/** @author Andrea Boriero */
public class PostgreSQL81IdentityColumnSupport extends IdentityColumnSupportImpl {
  @Override
  public boolean containDataTypeInIdentityColumn() {
    return true;
  }

  @Override
  public String getIdentityColumnString(Type type) {
    return type.equals(Long.class) || type.equals(long.class)
        ? "bigserial not null"
        : "serial not null";
  }
}
