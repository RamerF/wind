package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.config.WindConfiguration.DdlAuto;
import io.github.ramerf.wind.core.config.WindContext;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exporter.TableExporter;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.Types;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import javax.persistence.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * The type Entity helper.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/12
 */
@Slf4j
public class EntityHelper {
  /** 实体信息:{类全路径:EntityInfo} */
  private static final Map<String, EntityInfo> CLAZZ_ENTITY_MAP = new ConcurrentHashMap<>();

  private static WindContext windContext;

  public static void initital(final WindContext windContext) {
    EntityHelper.windContext = windContext;
  }

  /**
   * 初始化实体和表对应信息.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   */
  public static <T extends AbstractEntityPoJo> void initEntity(final Class<T> clazz) {
    final EntityInfo entityInfo =
        EntityInfo.of(
            clazz, windContext.getWindConfiguration(), windContext.getDbMetaData().getDialect());
    CLAZZ_ENTITY_MAP.put(clazz.getTypeName(), entityInfo);
    // 这里进行表定义更新
    ddlAuto(entityInfo);
  }

  /** 初始化关系映射. */
  public static void initEntityMapping() {
    CLAZZ_ENTITY_MAP.values().stream()
        .filter(o -> !o.getClazz().equals(AbstractEntityPoJo.class))
        .filter(o -> AbstractEntityPoJo.class.isAssignableFrom(o.getClazz()))
        .forEach(EntityMapping::initial);
  }

  /**
   * 获取lambda表达式对应的数据库表列名.
   *
   * @param function the function
   * @return the column
   */
  public static String getColumn(BeanFunction function) {
    if (log.isTraceEnabled()) {
      log.trace("getColumn:[{}]", CLAZZ_ENTITY_MAP);
    }
    return initEntityIfNeeded(function.getImplClassFullPath())
        .getFieldColumnMap()
        .get(function.getField().getName());
  }

  /**
   * 获取Field对应的jdbcType名称,用于sql设置值.<br>
   * 默认使用{@link Column#columnDefinition()}内的列定义,如果为空则使用默认值.
   *
   * @param field the field
   * @param defaultValue 默认值
   * @return jdbcType名称 jdbc type name
   * @see Types
   */
  public static String getJdbcTypeName(final Field field, final String defaultValue) {
    String typeName = defaultValue;
    final Column column = field.getAnnotation(Column.class);
    if (Objects.nonNull(column)) {
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

  /**
   * Gets entity info.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the entity info
   */
  public static <T extends AbstractEntity> EntityInfo getEntityInfo(@Nonnull final Class<T> clazz) {
    return initEntityIfNeeded(clazz);
  }

  private static <T extends AbstractEntity> EntityInfo initEntityIfNeeded(
      @Nonnull final Class<T> clazz) {
    final String fullPath = clazz.getTypeName();
    synchronized (EntityHelper.class) {
      final EntityInfo entityInfo = CLAZZ_ENTITY_MAP.get(fullPath);
      if (entityInfo == null || CollectionUtils.isEmpty(entityInfo.getFieldColumnMap())) {
        initEntity(BeanUtils.getClazz(fullPath));
      }
    }
    return CLAZZ_ENTITY_MAP.get(fullPath);
  }

  private static EntityInfo initEntityIfNeeded(final String fullPath) {
    synchronized (EntityHelper.class) {
      final EntityInfo entityInfo = CLAZZ_ENTITY_MAP.get(fullPath);
      if (entityInfo == null || CollectionUtils.isEmpty(entityInfo.getFieldColumnMap())) {
        initEntity(BeanUtils.getClazz(fullPath));
      }
    }
    return CLAZZ_ENTITY_MAP.get(fullPath);
  }

  private static void ddlAuto(final EntityInfo entityInfo) {
    final DdlAuto ddlAuto = windContext.getWindConfiguration().getDdlAuto();
    if (ddlAuto == null || DdlAuto.NONE.equals(ddlAuto)) {
      return;
    }
    if (!entityInfo.isMapToTable()) {
      return;
    }
    // 先删除,再创建
    if (DdlAuto.CREATE.equals(ddlAuto)) {
      // Phase 1. delete
      final JdbcTemplate jdbcTemplate = windContext.getJdbcTemplateExecutor().getJdbcTemplate();
      final String dropSql = "drop table if exists " + entityInfo.getName();
      log.info("ddlAuto:drop table[{}]", dropSql);
      jdbcTemplate.execute(dropSql);
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
   * <p>映射到数据库表需要实体包含注解中的一个: {@link Entity},{@link TableInfo}.
   */
  public static boolean isMapToTable(final Class<?> clazz) {
    return clazz != null
        && (clazz.getAnnotation(Entity.class) != null
            || clazz.getAnnotation(TableInfo.class) != null);
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    IFunction<AbstractEntityPoJo, Date> function = AbstractEntityPoJo::getCreateTime;
    log.info("main:[{}]", getColumn(function));
  }
}
