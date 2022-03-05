package io.github.ramerf.wind.core.autoconfig.jdbc;

import com.alibaba.druid.pool.DruidDataSource;
import com.alibaba.druid.pool.DruidDataSourceFactory;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.util.PropertyElf;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration.DataSourceConfig;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration.DataSourceConfig.DataSourceType;
import io.github.ramerf.wind.core.exception.DataSourceException;
import java.lang.reflect.Method;
import java.util.*;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import org.apache.commons.dbcp2.BasicDataSourceFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration.DataSourceConfig.DataSourceType.*;

/**
 * 数据源工厂.
 *
 * @author ramer
 * @since 2022.02.26
 */
public class DataSourceConfigurationFactory {
  @Nonnull
  public static DataSource getDataSource(final DataSourceConfig config) {
    final Map<String, String> map = config.getProperties();
    if (map.isEmpty()) {
      throw new DataSourceException("DataSource not defined");
    }
    final String firstKey = map.keySet().stream().filter(o -> o.contains(".")).findAny().orElse("");
    final DataSourceType type = getDataSourceType(firstKey);
    final Properties properties = new Properties();
    final String prefix = type.name().toLowerCase();
    map.forEach(
        (k, v) -> {
          if (k.startsWith(prefix)) {
            properties.put(k.substring(prefix.length() + 1), v);
          }
        });
    switch (type) {
      case DRUID:
        return getDruidDataSource(properties);
      case DBCP:
        return getDbcpDataSource(properties);
      case HIKARI:
      default:
        return getHikariDataSource(properties);
    }
  }

  private static DataSourceType getDataSourceType(final String key) {
    if (key.toUpperCase().startsWith(DRUID.name())) {
      return DRUID;
    } else if (key.toUpperCase().startsWith(DBCP.name())) {
      return DBCP;
    } else if (key.toUpperCase().startsWith(HIKARI.name())) {
      return HIKARI;
    } else {
      throw new IllegalArgumentException(
          "Unsupported DataSource type: " + key.substring(0, key.indexOf(".")));
    }
  }

  private static DataSource getDruidDataSource(final Properties properties) {
    DruidDataSource dataSource = new DruidDataSource();
    try {
      return DruidDataSourceFactory.createDataSource(properties);
    } catch (Exception e) {
      throw new DataSourceException(e);
    }
  }

  private static DataSource getDbcpDataSource(final Properties properties) {
    try {
      return BasicDataSourceFactory.createDataSource(properties);
    } catch (Exception e) {
      throw new DataSourceException(e);
    }
  }

  private static DataSource getHikariDataSource(final Properties properties) {
    final HikariConfig hikariConfig = new HikariConfig();
    List<Method> methods = Arrays.asList(HikariConfig.class.getMethods());
    properties.forEach(
        (key, value) -> {
          final String propName = key.toString();
          final Logger logger = LoggerFactory.getLogger(PropertyElf.class);

          // use the english locale to avoid the infamous turkish locale bug
          String methodName =
              "set" + propName.substring(0, 1).toUpperCase(Locale.ENGLISH) + propName.substring(1);
          Method writeMethod =
              methods.stream()
                  .filter(m -> m.getName().equals(methodName) && m.getParameterCount() == 1)
                  .findFirst()
                  .orElse(null);

          if (writeMethod == null) {
            String methodName2 = "set" + propName.toUpperCase(Locale.ENGLISH);
            writeMethod =
                methods.stream()
                    .filter(m -> m.getName().equals(methodName2) && m.getParameterCount() == 1)
                    .findFirst()
                    .orElse(null);
          }

          if (writeMethod == null) {
            logger.warn(
                "Property {} does not exist on target {}",
                propName,
                ((Object) hikariConfig).getClass());
            return;
          }

          try {
            Class<?> paramClass = writeMethod.getParameterTypes()[0];
            if (paramClass == int.class) {
              writeMethod.invoke(hikariConfig, Integer.parseInt(value.toString()));
            } else if (paramClass == long.class) {
              writeMethod.invoke(hikariConfig, Long.parseLong(value.toString()));
            } else if (paramClass == short.class) {
              writeMethod.invoke(hikariConfig, Short.parseShort(value.toString()));
            } else if (paramClass == boolean.class || paramClass == Boolean.class) {
              writeMethod.invoke(hikariConfig, Boolean.parseBoolean(value.toString()));
            } else if (paramClass == String.class) {
              writeMethod.invoke(hikariConfig, value.toString());
            } else {
              try {
                logger.debug("Try to create a new instance of \"{}\"", value);
                writeMethod.invoke(
                    hikariConfig,
                    Class.forName(value.toString()).getDeclaredConstructor().newInstance());
              } catch (InstantiationException | ClassNotFoundException e) {
                logger.debug(
                    "Class \"{}\" not found or could not instantiate it (Default constructor)",
                    value);
                writeMethod.invoke(hikariConfig, value);
              }
            }
          } catch (Exception ignored) {
            // 忽略无效的key
          }
        });
    try {
      return new HikariDataSource(hikariConfig);
    } catch (Exception e) {
      throw new DataSourceException(e);
    }
  }
}
