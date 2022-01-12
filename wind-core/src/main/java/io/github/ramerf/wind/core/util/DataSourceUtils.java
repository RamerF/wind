package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.executor.DataAccessException;
import java.sql.*;
import javax.sql.DataSource;

/**
 * @author ramer
 * @since 26/12/2021
 */
public class DataSourceUtils {
  public static Connection getConnection(DataSource dataSource) {
    Asserts.notNull(dataSource, "No DataSource specified");
    try {
      Connection connection = dataSource.getConnection();
      if (connection == null) {
        throw new IllegalStateException(
            "DataSource returned null from getConnection(): " + dataSource);
      }
      return connection;
    } catch (SQLException e) {
      throw new DataAccessException("Failed to obtain JDBC Connection", e);
    }
  }

  public static PreparedStatement preparedStatement(Connection connection, String sql)
      throws DataAccessException {
    Asserts.notNull(connection, "No Connection specified");
    try {
      PreparedStatement statement = connection.prepareStatement(sql);
      if (statement == null) {
        throw new DataAccessException("Failed to obtain PreparedStatement");
      }
      return statement;
    } catch (SQLException e) {
      release(connection);
      throw new DataAccessException("Failed to obtain PreparedStatement", e);
    }
  }

  public static PreparedStatement preparedStatement(
      Connection connection, String sql, int autoGeneratedKeys) {
    Asserts.notNull(connection, "No Connection specified");
    try {
      return connection.prepareStatement(sql, autoGeneratedKeys);
    } catch (SQLException e) {
      release(connection);
      throw new DataAccessException("Failed to obtain PreparedStatement", e);
    }
  }

  public static void release(Connection connection) {
    if (connection != null) {
      try {
        connection.close();
      } catch (SQLException e) {
        throw new IllegalStateException("Failed to release JDBC Connection", e);
      }
    }
  }

  public static void release(PreparedStatement preparedStatement) {
    if (preparedStatement != null) {
      try {
        preparedStatement.close();
      } catch (SQLException e) {
        throw new IllegalStateException("Failed to release preparedStatement", e);
      }
    }
  }

  public static void release(ResultSet resultSet) {
    if (resultSet != null) {
      try {
        resultSet.close();
      } catch (SQLException e) {
        throw new IllegalStateException("Failed to release ResultSet", e);
      }
    }
  }
}