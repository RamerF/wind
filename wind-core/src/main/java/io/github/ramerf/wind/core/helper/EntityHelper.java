package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.config.WindConfiguration.DdlAuto;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
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

import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.CREATE_TIME_FIELD_NAME;
import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.UPDATE_TIME_FIELD_NAME;

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

  public static WindConfiguration CONFIGURATION;

  /**
   * 初始化实体和表对应信息.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   */
  public static <T extends AbstractEntityPoJo> void initEntity(final Class<T> clazz) {
    Map<String, String> map = new HashMap<>(10);
    // 0:创建时间 1:更新时间
    final Field[] timeField = new Field[2];
    // 默认时间字段
    final Field[] defaultTimeField = new Field[2];
    EntityUtils.getAllColumnFields(clazz)
        .forEach(
            field -> {
              final Column columnAnnotation = field.getAnnotation(Column.class);
              if (field.getAnnotation(CreateTimestamp.class) != null) {
                timeField[0] = field;
              }
              if (field.getAnnotation(UpdateTimestamp.class) != null) {
                timeField[1] = field;
              }
              if (field.getName().equals(CREATE_TIME_FIELD_NAME)) {
                defaultTimeField[0] = field;
              }
              if (field.getName().equals(UPDATE_TIME_FIELD_NAME)) {
                defaultTimeField[1] = field;
              }
              final String column =
                  columnAnnotation != null && StringUtils.nonEmpty(columnAnnotation.name())
                      ? columnAnnotation.name()
                      : StringUtils.camelToUnderline(field.getName());
              map.put(field.getName(), column);
            });
    final EntityInfo entityInfo = EntityInfo.of(clazz, map, CONFIGURATION);
    entityInfo.setCreateTimeField(timeField[0] == null ? defaultTimeField[0] : timeField[0]);
    entityInfo.setUpdateTimeFiled(timeField[1] == null ? defaultTimeField[1] : timeField[1]);
    CLAZZ_ENTITY_MAP.put(clazz.getTypeName(), entityInfo);
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
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    StringBuilder sql = new StringBuilder("CREATE TABLE ").append(entityInfo.getName()).append(" ");
    for (EntityColumn column : columns) {
      if (column.getColumnDefinition() != null) {
        sql.append(column.getName()).append(column.getColumnDefinition());
      } else {
        // id bigint(10) (not null) default null primary key,
        sql.append(column.getName())
            .append(column.getSqlType())
            .append("(")
            .append(column.getSqlLengthDefinition())
            .append("(");
        // 还需要拼接sql类型  bigint
      }
    }
  }

  /** 更新数据库表定义. */
  private static void ddlUpdate(@Nonnull final EntityInfo entityInfo) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    for (EntityColumn column : columns) {
      throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
    }
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
