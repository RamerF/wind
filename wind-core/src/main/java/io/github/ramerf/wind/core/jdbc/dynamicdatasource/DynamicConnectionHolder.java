package io.github.ramerf.wind.core.jdbc.dynamicdatasource;

import io.github.ramerf.wind.core.jdbc.ConnectionHolder;
import io.github.ramerf.wind.core.jdbc.SimpleConnectionHolder;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 24/02/2022
 */
@Slf4j
public class DynamicConnectionHolder implements ConnectionHolder {
  private final Map<String, SimpleConnectionHolder> dataSourceMap = new ConcurrentHashMap<>();

  public DynamicConnectionHolder(@Nonnull DataSource dataSource) {
    dataSourceMap.put(
        DynamicDataSourceHolder.peek(),
        new SimpleConnectionHolder(DataSourceUtils.getConnection(dataSource)));
  }

  @Override
  public void setConnection(final Connection connection) {
    SimpleConnectionHolder connectionHolder = dataSourceMap.get(DynamicDataSourceHolder.peek());
    if (connectionHolder == null) {
      connectionHolder = new SimpleConnectionHolder(connection);
      dataSourceMap.put(DynamicDataSourceHolder.peek(), connectionHolder);
    }
    connectionHolder.setConnection(connection);
  }

  @Override
  public Connection getConnection() {
    final SimpleConnectionHolder connectionHolder =
        dataSourceMap.get(DynamicDataSourceHolder.peek());
    return connectionHolder == null ? null : connectionHolder.getConnection();
  }

  @Override
  public Connection requestConnection() {
    return dataSourceMap.get(DynamicDataSourceHolder.peek()).requestConnection();
  }

  @Override
  public void releaseConnection() {
    dataSourceMap.get(DynamicDataSourceHolder.peek()).releaseConnection();
  }
}
