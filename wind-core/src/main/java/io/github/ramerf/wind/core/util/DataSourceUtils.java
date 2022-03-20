package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.executor.DataAccessException;
import java.sql.*;
import javax.annotation.Nullable;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 26/12/2021
 */
@Slf4j
public class DataSourceUtils {
  public static Connection getConnection(DataSource dataSource) throws DataAccessException {
    Asserts.notNull(dataSource, "No DataSource specified");
    try {
      Connection connection = dataSource.getConnection();
      if (connection == null) {
        throw new DataAccessException(
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
      close(connection);
      throw new DataAccessException("Failed to obtain PreparedStatement", e);
    }
  }

  public static PreparedStatement preparedStatement(
      Connection connection, String sql, int autoGeneratedKeys) throws DataAccessException {
    Asserts.notNull(connection, "No Connection specified");
    try {
      return connection.prepareStatement(sql, autoGeneratedKeys);
    } catch (SQLException e) {
      close(connection);
      throw new DataAccessException("Failed to obtain PreparedStatement", e);
    }
  }

  public static boolean getAutoCommit(Connection connection) throws DataAccessException {
    if (connection != null) {
      try {
        return connection.getAutoCommit();
      } catch (SQLException e) {
        throw new DataAccessException("Failed to getAutoCommit from JDBC Connection", e);
      }
    }
    throw new DataAccessException("Failed to getAutoCommit from JDBC Connection for null");
  }

  public static boolean getAutoCommit(Connection connection, final boolean defaultValue)
      throws DataAccessException {
    try {
      return getAutoCommit(connection);
    } catch (DataAccessException e) {
      return defaultValue;
    }
  }

  public static void commit(Connection connection) throws DataAccessException {
    if (connection != null) {
      try {
        connection.commit();
      } catch (SQLException e) {
        throw new DataAccessException("Failed to Commit JDBC Connection", e);
      }
    }
  }

  public static void releaseConnection(@Nullable Connection connection) {
    try {
      close(connection);
    } catch (DataAccessException ex) {
      log.debug("Could not close JDBC Connection", ex);
    } catch (Throwable ex) {
      log.debug("Unexpected exception on closing JDBC Connection", ex);
    }
  }

  public static void close(Connection connection) throws DataAccessException {
    try {
      doClose(connection);
    } catch (SQLException e) {
      throw new DataAccessException("Failed to Release JDBC Connection", e);
    }
  }

  public static void doClose(Connection connection) throws SQLException {
    if (connection != null) {
      connection.close();
    }
  }

  public static boolean isClosed(Connection connection) {
    if (connection == null) {
      return true;
    }
    try {
      return connection.isClosed();
    } catch (SQLException e) {
      return true;
    }
  }

  public static void close(PreparedStatement preparedStatement) {
    if (preparedStatement != null) {
      try {
        preparedStatement.close();
      } catch (SQLException ex) {
        log.trace("Could not close JDBC PreparedStatement", ex);
      } catch (Throwable ex) {
        log.trace("Unexpected exception on closing JDBC PreparedStatement", ex);
      }
    }
  }

  public static void close(ResultSet resultSet) {
    if (resultSet != null) {
      try {
        resultSet.close();
      } catch (SQLException ex) {
        log.trace("Could not close JDBC ResultSet", ex);
      } catch (Throwable ex) {
        log.trace("Unexpected exception on closing JDBC ResultSet", ex);
      }
    }
  }

  public static void rollback(Connection connection) throws DataAccessException {
    if (connection != null) {
      try {
        connection.rollback();
      } catch (SQLException e) {
        throw new DataAccessException("Failed to Rolling back JDBC Connection", e);
      }
    }
  }
}
