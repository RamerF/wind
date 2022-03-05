package io.github.ramerf.wind.core.jdbc;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.util.Asserts;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import javax.annotation.Nullable;
import javax.sql.DataSource;

public class TransactionSynchronizationManager {
  private static final ThreadLocal<Map<DataSource, ConnectionHolder>> DATASOURCE_CONNECTION_HOLDER =
      ThreadLocal.withInitial(HashMap::new);

  public static ConnectionHolder getConnection(final DataSource dataSource) {
    Asserts.notNull(dataSource, "No DataSource specified");
    final Map<DataSource, ConnectionHolder> holderMap = DATASOURCE_CONNECTION_HOLDER.get();
    ConnectionHolder connectionHolder = holderMap.get(dataSource);
    if (connectionHolder != null) {
      Connection connection = connectionHolder.getConnection();
      if (connection == null || DataSourceUtils.isClosed(connection)) {
        connectionHolder.setConnection(connection = DataSourceUtils.getConnection(dataSource));
      }
      connectionHolder.requestConnection();
      return connectionHolder;
    }
    final Connection connection = DataSourceUtils.getConnection(dataSource);
    connectionHolder = new ConnectionHolder(connection);
    holderMap.put(dataSource, connectionHolder);
    connectionHolder.requestConnection();
    return connectionHolder;
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
