package io.github.ramerf.wind.core.jdbc;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.util.Asserts;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nullable;
import javax.sql.DataSource;

public class TransactionSynchronizationManager {
  private static final ThreadLocal<Map<DataSource, ConnectionHolder>> DATASOURCE_CONNECTION_HOLDER =
      ThreadLocal.withInitial(ConcurrentHashMap::new);

  /** 从数据源获取连接,<b>同时连接引用数加1</b> */
  public static ConnectionHolder getConnectionHolder(final DataSource dataSource) {
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
