package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.exception.TooManyResultException;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.util.*;
import java.sql.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * The jdbc template executor.
 *
 * @author ramer
 * @since 2020/5/19
 */
@Slf4j
@SuppressWarnings("DuplicatedCode")
public class SimpleJdbcExecutor implements Executor {
  private DataSource dataSource;

  public SimpleJdbcExecutor() {}

  public SimpleJdbcExecutor(DataSource dataSource) {
    this.dataSource = dataSource;
  }

  @Override
  public <T, R> R fetchOne(@Nonnull final SqlParam<T> sqlParam) throws DataAccessException {
    return fetchOne(sqlParam, null);
  }

  @Override
  public <T, R> R fetchOne(@Nonnull final SqlParam<T> sqlParam, ResultHandler<R> resultHandler)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          @SuppressWarnings("unchecked")
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          final List<R> result =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                  ResultHandlerUtil.getResultHandler(clazz, resultHandler));
          if (result.isEmpty()) {
            return null;
          }
          if (result.size() > 1) {
            throw new TooManyResultException(result.size());
          }
          return result.get(0);
        });
  }

  @Override
  public <T, R> List<R> fetchAll(@Nonnull final SqlParam<T> sqlParam, final Class<R> clazz)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () ->
            query(
                sqlParam,
                ps -> sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                ResultHandlerUtil.getResultHandler(clazz)));
  }

  @Override
  public <T, R> Page<R> fetchPage(
      @Nonnull final SqlParam<T> sqlParam, final long total, final Pageable page)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          @SuppressWarnings("unchecked")
          Class<R> clazz = (Class<R>) sqlParam.clazz;
          final List<R> list =
              total < 1
                  ? null
                  : query(
                      sqlParam,
                      ps ->
                          sqlParam
                              .condition
                              .getValues(sqlParam.startIndex)
                              .forEach(o -> o.accept(ps)),
                      ResultHandlerUtil.getResultHandler(clazz));
          final Pageable pageable = page == null ? PageRequest.of(0, Integer.MAX_VALUE) : page;
          // 从0开始
          final int currentPage = pageable.getPageNumber();
          // 每页大小
          final int pageSize = pageable.getPageSize();
          return PageUtils.toPage(list, total, currentPage, pageSize, pageable.getSort());
        });
  }

  @Override
  public <T> long fetchCount(@Nonnull final SqlParam<T> sqlParam) {
    return aroundRead(
        sqlParam,
        () -> {
          List<Long> result =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                  new PrimitiveResultHandler<>(Long.class));
          if (result.isEmpty()) {
            return 0L;
          }
          return result.get(0);
        });
  }

  @Override
  public <T, R> R queryForObject(@Nonnull final SqlParam<?> sqlParam, final Object[] args)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          @SuppressWarnings("unchecked")
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          final List<R> result =
              query(
                  sqlParam,
                  ps -> {
                    AtomicInteger startIndex = sqlParam.startIndex;
                    for (Object arg : args) {
                      JdbcUtils.setObject(ps, startIndex.getAndIncrement(), arg);
                    }
                  },
                  ResultHandlerUtil.getResultHandler(clazz));
          if (result.isEmpty()) {
            return null;
          }
          if (result.size() > 1) {
            throw new TooManyResultException(result.size());
          }
          return result.get(0);
        });
  }

  @Override
  public Map<String, Object> queryForMap(@Nonnull final SqlParam<?> sqlParam, final Object... args)
      throws DataAccessException {
    List<Map<String, Object>> maps = queryForList(sqlParam, args);
    return maps.isEmpty() ? null : maps.get(0);
  }

  @Override
  public List<Map<String, Object>> queryForList(
      @Nonnull final SqlParam<?> sqlParam, final Object... args) throws DataAccessException {
    return aroundRead(
        sqlParam,
        () ->
            query(
                sqlParam,
                ps -> {
                  AtomicInteger startIndex = sqlParam.startIndex;
                  for (Object arg : args) {
                    JdbcUtils.setObject(ps, startIndex.getAndIncrement(), arg);
                  }
                },
                new MapResultHandler()));
  }

  @Override
  public <T> List<T> query(
      @Nonnull final SqlParam<?> sqlParam,
      final PreparedStatementSetter pss,
      final ResultHandler<T> resultHandler)
      throws DataAccessException {
    Connection connection = DataSourceUtils.getConnection(dataSource);
    PreparedStatement ps = DataSourceUtils.preparedStatement(connection, sqlParam.sql);
    if (pss != null) {
      pss.setValues(ps);
    }
    ResultSet resultSet;
    try {
      resultSet = ps.executeQuery();
    } catch (SQLException e) {
      DataSourceUtils.release(connection);
      throw new DataAccessException("Fail to execute query:" + sqlParam.sql, e);
    }
    List<T> ts = new ArrayList<>();
    try {
      while (resultSet.next()) {
        ts.add(resultHandler.handle(resultSet));
      }
    } catch (SQLException e) {
      throw new DataAccessException("Fail to handle resultSet:" + sqlParam.sql, e);
    } finally {
      DataSourceUtils.release(resultSet);
      DataSourceUtils.release(ps);
      DataSourceUtils.release(connection);
    }
    return ts;
  }

  @Override
  public int update(final PreparedStatementCreator psc, final KeyHolder generatedKeyHolder)
      throws DataAccessException {
    return aroundWrite(
        () -> {
          Connection connection = DataSourceUtils.getConnection(dataSource);
          PreparedStatement ps = psc.createPreparedStatement(connection);
          try {
            List<Map<String, Object>> generatedKeys = generatedKeyHolder.getKeyList();
            generatedKeys.clear();

            int rows = ps.executeUpdate();
            MapResultHandler resultHandler = new MapResultHandler();
            ResultSet resultSet = ps.getGeneratedKeys();
            while (resultSet.next()) {
              generatedKeys.add(resultHandler.handle(resultSet));
            }
            return rows;
          } catch (SQLException e) {
            throw new DataAccessException("Fail to exexute update", e);
          } finally {
            DataSourceUtils.release(ps);
            DataSourceUtils.release(connection);
          }
        });
  }

  @Override
  public int[] batchUpdate(String sql, final BatchPreparedStatementSetter pss)
      throws DataAccessException {
    return aroundWrite(
        () -> {
          Connection connection = DataSourceUtils.getConnection(dataSource);
          PreparedStatement ps = DataSourceUtils.preparedStatement(connection, sql);
          int batchSize = pss.getBatchSize();
          try {
            if (JdbcUtils.supportsBatchUpdates(connection)) {
              for (int i = 0; i < batchSize; i++) {
                pss.setValues(ps, i);
                ps.addBatch();
              }
              return ps.executeBatch();
            } else {
              int[] rowsAffectedArray = new int[batchSize];
              for (int i = 0; i < batchSize; i++) {
                pss.setValues(ps, i);
                rowsAffectedArray[i] = ps.executeUpdate();
              }
              return rowsAffectedArray;
            }
          } catch (SQLException e) {
            throw new DataAccessException("Fail to execute batch update", e);
          } finally {
            DataSourceUtils.release(ps);
            DataSourceUtils.release(connection);
          }
        });
  }

  @Override
  public int[] batchUpdate(
      final PreparedStatementCreator psc,
      final BatchPreparedStatementSetter pss,
      final KeyHolder generatedKeyHolder)
      throws org.springframework.dao.DataAccessException {
    Connection connection = DataSourceUtils.getConnection(dataSource);
    PreparedStatement ps = psc.createPreparedStatement(connection);
    ResultSet resultSet = null;
    int batchSize = pss.getBatchSize();
    try {
      if (JdbcUtils.supportsBatchUpdates(connection)) {
        for (int i = 0; i < batchSize; i++) {
          pss.setValues(ps, i);
          ps.addBatch();
        }
        // TODO WARN 批量创建，返回主键
        final int[] rows = ps.executeBatch();
        List<Map<String, Object>> generatedKeys = generatedKeyHolder.getKeyList();
        generatedKeys.clear();
        MapResultHandler resultHandler = new MapResultHandler();
        resultSet = ps.getGeneratedKeys();
        while (resultSet.next()) {
          generatedKeys.add(resultHandler.handle(resultSet));
        }
        return rows;
      } else {
        int[] rowsAffectedArray = new int[batchSize];
        for (int i = 0; i < batchSize; i++) {
          pss.setValues(ps, i);
          rowsAffectedArray[i] = ps.executeUpdate();
        }
        return rowsAffectedArray;
      }
    } catch (SQLException e) {
      throw new DataAccessException("Fail to execute batch update", e);
    } finally {
      DataSourceUtils.release(resultSet);
      DataSourceUtils.release(ps);
      DataSourceUtils.release(connection);
    }
  }

  @Override
  public JdbcTemplate getJdbcTemplate() {
    return null;
  }

  @Override
  public int update(String sql, @Nonnull PreparedStatementSetter pss) throws DataAccessException {
    return aroundWrite(
        () -> {
          Connection connection = DataSourceUtils.getConnection(dataSource);
          PreparedStatement ps = DataSourceUtils.preparedStatement(connection, sql);
          try {
            pss.setValues(ps);
            return ps.executeUpdate();
          } catch (SQLException e) {
            throw new DataAccessException("Fail to exexute update", e);
          } finally {
            DataSourceUtils.release(ps);
            DataSourceUtils.release(connection);
          }
        });
  }

  private <T> T aroundRead(@Nonnull final SqlParam<?> sqlParam, Supplier<T> supplier) {
    return supplier.get();
  }

  private <T> T aroundWrite(Supplier<T> supplier) {
    return supplier.get();
  }

  private <T> T aroundWrite(@Nonnull final Class<?> clazz, Supplier<T> supplier) {
    return supplier.get();
  }
}
