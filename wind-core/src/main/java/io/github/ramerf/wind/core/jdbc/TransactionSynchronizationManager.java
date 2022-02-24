package io.github.ramerf.wind.core.jdbc;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.util.Asserts;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.springframework.lang.Nullable;

public class TransactionSynchronizationManager {
  private static final ThreadLocal<Map<DataSource, ConnectionHolder>> DATASOURCE_CONNECTION_HOLDER =
      ThreadLocal.withInitial(HashMap::new);

  public static Connection getConnection(final DataSource dataSource) {
    Asserts.notNull(dataSource, "No DataSource specified");
    final Map<DataSource, ConnectionHolder> holderMap = DATASOURCE_CONNECTION_HOLDER.get();
    ConnectionHolder connectionHolder = holderMap.get(dataSource);
    if (connectionHolder != null) {
      Connection connection = connectionHolder.getConnection();
      if (connection == null) {
        connectionHolder.setConnection(connection = DataSourceUtils.getConnection(dataSource));
      }
      connectionHolder.requestConnection();
      return connection;
    }
    final Connection connection = DataSourceUtils.getConnection(dataSource);
    connectionHolder = new ConnectionHolder(connection);
    holderMap.put(dataSource, connectionHolder);
    connectionHolder.requestConnection();
    return connection;
  }

  public static void releaseConnection(
      @Nullable Connection connection, @Nullable DataSource dataSource) {
    if (connection == null) {
      return;
    }
    if (dataSource != null) {
      final Map<DataSource, ConnectionHolder> holderMap = DATASOURCE_CONNECTION_HOLDER.get();
      ConnectionHolder connectionHolder = holderMap.get(dataSource);
      if (connectionHolder != null) {
        connectionHolder.releaseConnection();
        return;
      }
    }
    try {
      DataSourceUtils.doClose(connection);
    } catch (SQLException exception) {
      throw new DataAccessException("Fail to Release Connection", exception);
    }
  }
}
