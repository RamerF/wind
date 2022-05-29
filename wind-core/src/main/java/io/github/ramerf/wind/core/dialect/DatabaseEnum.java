package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.dialect.mysql.*;
import io.github.ramerf.wind.core.dialect.postgresql.*;
import io.github.ramerf.wind.core.dialect.sqlite.SqliteDialect;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import lombok.extern.slf4j.Slf4j;

/**
 * 支持的数据库.
 *
 * @since 2020.08.16
 * @author ramer
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
    public Dialect resolveDialect(DatabaseMetaData metaData) {
      try {
        String databaseName = metaData.getDatabaseProductName();
        if ("MySQL".equals(databaseName)) {
          int majorVersion = metaData.getDatabaseMajorVersion();
          int minorVersion = metaData.getDatabaseMinorVersion();
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
    public Dialect resolveDialect(DatabaseMetaData metaData) {
      try {
        String databaseName = metaData.getDatabaseProductName();
        if ("PostgreSQL".equals(databaseName)) {
          int majorVersion = metaData.getDatabaseMajorVersion();
          int minorVersion = metaData.getDatabaseMinorVersion();
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
  SQLITE {
    @Override
    public Class<? extends Dialect> latestDialect() {
      return SqliteDialect.class;
    }

    @Override
    public Dialect resolveDialect(DatabaseMetaData metaData) {
      try {
        String databaseName = metaData.getDatabaseProductName();
        if ("SQLite".equals(databaseName)) {
          int majorVersion = metaData.getDatabaseMajorVersion();
          int minorVersion = metaData.getDatabaseMinorVersion();
          return new SqliteDialect();
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
   * @param metaData the meta data
   * @return the dialect
   */
  public abstract Dialect resolveDialect(DatabaseMetaData metaData);

  private static Dialect latestDialectInstance(DatabaseEnum database) {
    return BeanUtils.initial(database.latestDialect());
  }
}
