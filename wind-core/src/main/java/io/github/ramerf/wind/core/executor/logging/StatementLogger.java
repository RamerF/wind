/**
 * Copyright 2009-2020 the original author or authors.
 *
 * <p>Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * <p>http://www.apache.org/licenses/LICENSE-2.0
 *
 * <p>Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.ramerf.wind.core.executor.logging;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.*;
import java.sql.ResultSet;
import java.sql.Statement;

/**
 * Statement proxy to add logging.
 *
 * @author Clinton Begin
 * @author Eduardo Macarron
 */
public final class StatementLogger extends BaseJdbcLogger implements InvocationHandler {

  private final Statement statement;

  private StatementLogger(Statement stmt, Log statementLog, int queryStack) {
    super(statementLog, queryStack);
    this.statement = stmt;
  }

  @Override
  public Object invoke(Object proxy, Method method, Object[] params) throws Throwable {
    try {
      if (Object.class.equals(method.getDeclaringClass())) {
        return method.invoke(this, params);
      }
      if (EXECUTE_METHODS.contains(method.getName())) {
        if (isDebugEnabled()) {
          debug(" Executing: " + removeExtraWhitespace((String) params[0]), true);
        }
        if ("executeQuery".equals(method.getName())) {
          ResultSet rs = (ResultSet) method.invoke(statement, params);
          return rs == null ? null : ResultSetLogger.newInstance(rs, log, queryStack);
        } else {
          return method.invoke(statement, params);
        }
      } else if ("getResultSet".equals(method.getName())) {
        ResultSet rs = (ResultSet) method.invoke(statement, params);
        return rs == null ? null : ResultSetLogger.newInstance(rs, log, queryStack);
      } else {
        return method.invoke(statement, params);
      }
    } catch (Throwable t) {
      throw ExceptionUtil.unwrapThrowable(t);
    }
  }

  /**
   * Creates a logging version of a Statement.
   *
   * @param stmt the statement
   * @param statementLog the statement log
   * @param queryStack the query stack
   * @return the proxy
   */
  public static Statement newInstance(Statement stmt, Log statementLog, int queryStack) {
    InvocationHandler handler = new StatementLogger(stmt, statementLog, queryStack);
    ClassLoader cl = Statement.class.getClassLoader();
    return (Statement) Proxy.newProxyInstance(cl, new Class[] {Statement.class}, handler);
  }

  /**
   * return the wrapped statement.
   *
   * @return the statement
   */
  public Statement getStatement() {
    return statement;
  }
}