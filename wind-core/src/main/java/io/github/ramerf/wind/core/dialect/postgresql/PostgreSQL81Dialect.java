/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.postgresql;

import io.github.ramerf.wind.core.dialect.Dialect;

/**
 * An SQL dialect for Postgres
 *
 * <p>For discussion of BLOB support in Postgres, as of 8.4, have a peek at <a
 * href="http://jdbc.postgresql.org/documentation/84/binary-data.html">http://jdbc.postgresql.org/documentation/84/binary-data.html</a>.
 * For the effects in regards to Hibernate see <a
 * href="http://in.relation.to/15492.lace">http://in.relation.to/15492.lace</a>
 *
 * @author Gavin King
 */
@SuppressWarnings("deprecation")
public class PostgreSQL81Dialect extends Dialect {}
