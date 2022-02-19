package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindContext;
import io.github.ramerf.wind.core.entity.TestLambda;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.exporter.TableExporter;
import io.github.ramerf.wind.core.function.FieldFunction;
import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.Types;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Entity helper.
 *
 * @author ramer
 * @since 2020 /5/12
 */
@Slf4j
public class EntityHelper {
  /** 实体信息:{类全路径:EntityInfo} */
  private static final Map<Class<?>, EntityInfo> CLAZZ_ENTITY_MAP = new ConcurrentHashMap<>();

  private static WindContext windContext;

  public static void initial(final WindContext windContext) {
    EntityHelper.windContext = windContext;
  }

  /**
   * 初始化实体和表对应信息.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   */
  public static <T> void initEntity(final Class<T> clazz) {
    final EntityInfo entityInfo =
        EntityInfo.of(
            clazz, windContext.getConfiguration(), windContext.getDbMetaData().getDialect());
    CLAZZ_ENTITY_MAP.put(clazz, entityInfo);
    // 这里进行表定义更新
    ddlAuto(entityInfo);
  }

  /** 初始化关系映射. */
  public static void initEntityMapping() {
    CLAZZ_ENTITY_MAP.values().stream()
        .filter(o -> o.getClazz().isAnnotationPresent(TableInfo.class))
        .forEach(EntityMapping::initial);
    EntityMapping.valid(CLAZZ_ENTITY_MAP);
  }

  /**
   * 获取lambda表达式对应的数据库表列名.
   *
   * @param function the function
   * @return the column
   */
  public static String getColumn(FieldFunction function) {
    if (log.isTraceEnabled()) {
      log.trace("getColumn:[{}]", CLAZZ_ENTITY_MAP);
    }
    final EntityColumn entityColumn =
        initEntityIfNeeded(function.getImplClassFullPath())
            .getFieldColumnMap()
            .get(function.getField());
    return entityColumn == null ? null : entityColumn.getName();
  }

  /**
   * 获取Field对应的jdbcType名称,用于sql设置值.<br>
   * 默认使用{@link TableColumn#columnDefinition()}内的列定义,如果为空则使用默认值.
   *
   * @param field the field
   * @param defaultValue 默认值
   * @return jdbcType名称 jdbc type name
   * @see Types
   */
  public static String getJdbcTypeName(final Field field, final String defaultValue) {
    String typeName = defaultValue;
    final TableColumn column = field.getAnnotation(TableColumn.class);
    if (column != null) {
      final String columnDefinition = column.columnDefinition();
      if (StringUtils.nonEmpty(columnDefinition)) {
        final String trim = StringUtils.trimWhitespace(columnDefinition);
        String definition = trim.contains(" ") ? trim.substring(0, trim.indexOf(" ")) : trim;
        final String leftParenthesis = "(";
        final String leftSquareBracket = "[";
        if (definition.contains(leftParenthesis)) {
          typeName = definition.substring(0, definition.indexOf(leftParenthesis));
        } else if (definition.contains(leftSquareBracket)) {
          typeName = definition.substring(0, definition.indexOf(leftSquareBracket));
        } else {
          typeName = definition;
        }
      }
    }
    return typeName;
  }

  public static <T> EntityInfo getEntityInfo(@Nonnull final Class<T> clazz) {
    return initEntityIfNeeded(clazz);
  }

  /**
   * 获取实体的主键字段.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the entity id field
   */
  public static <T> Field getEntityIdField(@Nonnull final Class<T> clazz) {
    return getEntityInfo(clazz).getIdColumn().getField();
  }

  private static <T> EntityInfo initEntityIfNeeded(@Nonnull final Class<T> clazz) {
    final String fullPath = clazz.getTypeName();
    synchronized (EntityHelper.class) {
      final EntityInfo entityInfo = CLAZZ_ENTITY_MAP.get(clazz);
      if (entityInfo == null || CollectionUtils.isEmpty(entityInfo.getFieldColumnMap())) {
        initEntity(clazz);
      }
    }
    return CLAZZ_ENTITY_MAP.get(clazz);
  }

  private static EntityInfo initEntityIfNeeded(final String fullPath) {
    final Class<?> clazz = BeanUtils.getClazz(fullPath);
    synchronized (EntityHelper.class) {
      final EntityInfo entityInfo = CLAZZ_ENTITY_MAP.get(clazz);
      if (entityInfo == null || CollectionUtils.isEmpty(entityInfo.getFieldColumnMap())) {
        initEntity(clazz);
      }
    }
    return CLAZZ_ENTITY_MAP.get(clazz);
  }

  private static void ddlAuto(final EntityInfo entityInfo) {
    final DdlAuto ddlAuto = windContext.getConfiguration().getDdlAuto();
    if (ddlAuto == null || DdlAuto.NONE.equals(ddlAuto)) {
      return;
    }
    if (!entityInfo.isMapToTable()) {
      return;
    }
    // 先删除,再创建
    if (DdlAuto.CREATE.equals(ddlAuto)) {
      // Phase 1. delete
      final Executor executor = windContext.getExecutor();
      final String dropSql = "drop table if exists " + entityInfo.getName();
      log.info("ddlAuto:drop table[{}]", dropSql);
      // TODO WARN 执行sql
      // executor.execute(dropSql);
      // Phase 2. create
      TableExporter.of(windContext).createTable(entityInfo);
    } else if (DdlAuto.UPDATE.equals(ddlAuto)) {
      // 仅新增列，不支持更新列定义
      TableExporter.of(windContext).updateTable(entityInfo);
    }
  }

  /**
   * 判断一个Class是否映射到数据库表.true:是
   *
   * <p>映射到数据库表需要实体包含注解: {@link TableInfo}.
   */
  public static boolean isMapToTable(final Class<?> clazz) {
    return clazz != null && clazz.isAnnotationPresent(TableInfo.class);
  }

  public static void main(String[] args) {
    GetterFunction<TestLambda, String> function = TestLambda::getName;
    log.info("main:[{}]", getColumn(function));
  }
}
