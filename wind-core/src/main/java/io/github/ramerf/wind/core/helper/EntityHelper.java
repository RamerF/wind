package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.config.WindConfiguration.DdlAuto;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.support.DdlAdapter;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.Types;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import javax.persistence.Entity;
import lombok.extern.slf4j.Slf4j;

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

  /** The constant CONFIGURATION. */
  public static WindConfiguration CONFIGURATION;

  /**
   * 初始化实体和表对应信息.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   */
  public static <T extends AbstractEntityPoJo> void initEntity(final Class<T> clazz) {
    CLAZZ_ENTITY_MAP.put(clazz.getTypeName(), EntityInfo.of(clazz, CONFIGURATION));
    // 这里进行表定义更新
    ddlAuto();
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

  private static void ddlAuto() {
    final DdlAuto ddlAuto = CONFIGURATION.getDdlAuto();
    // 先删除,再创建
    if (DdlAuto.CREATE.equals(ddlAuto)) {
      CLAZZ_ENTITY_MAP.values().stream()
          .filter(EntityHelper::isMapToTable)
          .forEach(EntityHelper::ddlCreate);
    }
    // 第一版本 仅新增列,后面支持更新列定义
    if (DdlAuto.UPDATE.equals(ddlAuto)) {
      CLAZZ_ENTITY_MAP.values().stream()
          .filter(EntityHelper::isMapToTable)
          .forEach(EntityHelper::ddlUpdate);
    }
  }

  /**
   * 判断一个{@link EntityInfo}是否映射到数据库表.true:是
   *
   * <p>映射到数据库表需要实体包含注解中的一个: {@link Entity},{@link TableInfo}.
   */
  private static boolean isMapToTable(final EntityInfo entityInfo) {
    return entityInfo != null
        && (entityInfo.getClazz().getAnnotation(Entity.class) != null
            || entityInfo.getClazz().getAnnotation(TableInfo.class) != null);
  }

  /** 删除表后,再新建数据库表. */
  private static void ddlCreate(@Nonnull final EntityInfo entityInfo) {
    new DdlAdapter().createTable(entityInfo);
  }

  /** 更新数据库表定义. */
  private static void ddlUpdate(@Nonnull final EntityInfo entityInfo) {
    new DdlAdapter().updateTable(entityInfo);
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
