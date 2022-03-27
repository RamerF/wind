package io.github.ramerf.wind.core.jdbc.dynamicdatasource;

import io.github.ramerf.wind.core.jdbc.ConnectionHolder;
import io.github.ramerf.wind.core.jdbc.TransactionSynchronizationManager;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import javax.sql.DataSource;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

/**
 * 支持动态数据源.
 *
 * @author ramer
 * @since 2022.03.20
 */
@Slf4j
@ToString
public class DynamicDataSource implements DataSource {
  @Setter private boolean strict = false;
  @Getter private String primary = "master";
  private final Map<String, DataSource> dataSourceMap = new ConcurrentHashMap<>();

  public void setPrimary(final String primary) {
    this.primary = primary;
    // 设置默认主库
    DynamicDataSourceHolder.clearPush(primary);
  }

  /**
   * 添加数据源
   *
   * @param key 数据源名称
   * @param dataSource 数据源
   */
  public synchronized void addDataSource(String key, DataSource dataSource) {
    DataSource oldDataSource = dataSourceMap.put(key, dataSource);
    // 关闭老的数据源
    if (oldDataSource != null) {
      closeDataSource(key, oldDataSource);
    }
    log.info("dynamic-datasource - add a datasource named [{}] success", key);
  }

  private void closeDataSource(String ds, DataSource dataSource) {
    try {
      Method closeMethod = BeanUtils.getDeclaredMethod(dataSource.getClass(), "close");
      if (closeMethod != null) {
        closeMethod.invoke(dataSource);
      }
    } catch (Exception e) {
      log.warn("dynamic-datasource closed datasource named [{}] failed", ds, e);
    }
  }

  public DataSource determineDataSource() {
    String key = DynamicDataSourceHolder.peek();
    return getDataSource(key);
  }

  public DataSource getDataSource(String key) {
    if (StringUtils.isEmpty(key)) {
      return determinePrimaryDataSource();
    }
    if (dataSourceMap.containsKey(key)) {
      log.debug("dynamic-datasource switch to the datasource named [{}]", key);
      return dataSourceMap.get(key);
    }
    if (strict) {
      throw new DataSourceNotFoundException(
          "dynamic-datasource could not find a datasource named" + key);
    }
    return determinePrimaryDataSource();
  }

  private DataSource determinePrimaryDataSource() {
    log.debug("dynamic-datasource switch to the primary datasource");
    DataSource dataSource = dataSourceMap.get(primary);
    if (dataSource != null) {
      return dataSource;
    }
    throw new DataSourceNotFoundException("dynamic-datasource can not find primary datasource");
  }

  @Override
  public Connection getConnection() throws SQLException {
    final ConnectionHolder connectionHolder =
        TransactionSynchronizationManager.getConnectionHolder(determineDataSource());
    return connectionHolder.getConnection();
  }

  @Override
  public Connection getConnection(String username, String password) throws SQLException {
    return determineDataSource().getConnection(username, password);
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T> T unwrap(Class<T> iface) throws SQLException {
    if (iface.isInstance(this)) {
      return (T) this;
    }
    return determineDataSource().unwrap(iface);
  }

  @Override
  public boolean isWrapperFor(Class<?> iface) throws SQLException {
    return (iface.isInstance(this) || determineDataSource().isWrapperFor(iface));
  }

  @Override
  public int getLoginTimeout() throws SQLException {
    return 0;
  }

  @Override
  public void setLoginTimeout(int timeout) throws SQLException {
    throw new UnsupportedOperationException("setLoginTimeout");
  }

  @Override
  public PrintWriter getLogWriter() {
    throw new UnsupportedOperationException("getLogWriter");
  }

  @Override
  public void setLogWriter(PrintWriter pw) throws SQLException {
    throw new UnsupportedOperationException("setLogWriter");
  }

  @Override
  public Logger getParentLogger() {
    return Logger.getLogger("global");
  }
}
