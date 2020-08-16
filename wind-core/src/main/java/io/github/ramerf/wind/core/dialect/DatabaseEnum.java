package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.dialect.mysql.*;
import io.github.ramerf.wind.core.dialect.postgresql.*;
import io.github.ramerf.wind.core.exception.CommonException;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import lombok.extern.slf4j.Slf4j;

/**
 * 支持的数据库.
 *
 * @since 2020.08.16
 * @author Tang Xiaofeng
 */
@Slf4j
public enum DatabaseEnum {
  /** The MYSQL. */
  MYSQL {
    @Override
    public Class<? extends Dialect> latestDialect() {
      return MySQL57Dialect.class;
    }

    @Override
    public Dialect resolveDialect(DatabaseMetaData info) {
      try {
        String databaseName = info.getDatabaseProductName();
        if ("MySQL".equals(databaseName)) {
          int majorVersion = info.getDatabaseMajorVersion();
          int minorVersion = info.getDatabaseMinorVersion();
          if (majorVersion < 5) {
            return new MySQLDialect();
          } else if (majorVersion == 5) {
            if (minorVersion < 5) {
              return new MySQL5Dialect();
            } else {
              return minorVersion < 7 ? new MySQL55Dialect() : new MySQL57Dialect();
            }
          } else {
            return latestDialectInstance(this);
          }
        } else {
          return null;
        }
      } catch (SQLException throwables) {
        throwables.printStackTrace();
        return null;
      }
    }
  },
  /** The Postgresql. */
  POSTGRESQL {
    @Override
    public Class<? extends Dialect> latestDialect() {
      return PostgreSQL95Dialect.class;
    }

    @Override
    public Dialect resolveDialect(DatabaseMetaData info) {
      try {
        String databaseName = info.getDatabaseProductName();
        if ("PostgreSQL".equals(databaseName)) {
          int majorVersion = info.getDatabaseMajorVersion();
          int minorVersion = info.getDatabaseMinorVersion();
          if (majorVersion < 8) {
            return new PostgreSQL81Dialect();
          } else if (majorVersion == 8) {
            return minorVersion >= 2 ? new PostgreSQL82Dialect() : new PostgreSQL81Dialect();
          } else {
            if (majorVersion == 9) {
              if (minorVersion < 2) {
                return new PostgreSQL9Dialect();
              }

              if (minorVersion < 4) {
                return new PostgreSQL92Dialect();
              }

              if (minorVersion < 5) {
                return new PostgreSQL94Dialect();
              }

              if (minorVersion < 6) {
                return new PostgreSQL95Dialect();
              }
            }

            return latestDialectInstance(this);
          }
        } else {
          return null;
        }
      } catch (Exception e) {
        log.warn(e.getMessage());
        log.error(e.getMessage(), e);
        return null;
      }
    }
  },
  ;

  DatabaseEnum() {}

  /**
   * Latest dialect class.
   *
   * @return the class
   */
  public abstract Class<? extends Dialect> latestDialect();

  /**
   * Resolve dialect dialect.
   *
   * @param var1 the var 1
   * @return the dialect
   */
  public abstract Dialect resolveDialect(DatabaseMetaData var1);

  private static Dialect latestDialectInstance(DatabaseEnum database) {
    try {
      return database.latestDialect().newInstance();
    } catch (IllegalAccessException | InstantiationException var2) {
      throw CommonException.of(var2);
    }
  }
}
