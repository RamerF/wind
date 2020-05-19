package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.Types;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.persistence.Column;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Entity helper.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/12
 */
@Slf4j
public class EntityHelper {
  /** 类全路径:{field:column} */
  private static final Map<String, Map<String, String>> FIELD_COLUMN_MAP =
      new ConcurrentHashMap<>();

  /**
   * 初始化实体和表对应信息.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   */
  public static <T extends AbstractEntityPoJo> void initEntity(final Class<T> clazz) {
    Map<String, String> map = new HashMap<>(10);
    EntityUtils.getAllColumnFields(clazz)
        .forEach(
            field -> {
              final Column columnAnnotation = field.getAnnotation(Column.class);
              final String column =
                  columnAnnotation != null && StringUtils.nonEmpty(columnAnnotation.name())
                      ? columnAnnotation.name()
                      : StringUtils.camelToUnderline(field.getName());
              map.put(field.getName(), column);
              FIELD_COLUMN_MAP.put(clazz.getTypeName(), map);
            });
  }

  /**
   * 获取lambda表达式对应的数据库表列名.
   *
   * @param function the function
   * @return the column
   */
  public static String getColumn(BeanFunction function) {
    if (log.isDebugEnabled()) {
      log.debug("getColumn:[{}]", FIELD_COLUMN_MAP);
    }
    final Map<String, String> fieldColumnMap =
        FIELD_COLUMN_MAP.get(function.getImplClassFullPath());
    if (CollectionUtils.isEmpty(fieldColumnMap)) {
      // 处理实体信息未自动扫描到的情况
      initEntity(BeanUtils.getClazz(function.getImplClassFullPath()));
      return FIELD_COLUMN_MAP
          .get(function.getImplClassFullPath())
          .get(function.getField().getName());
    }
    return fieldColumnMap.get(function.getField().getName());
  }

  /**
   * 获取Field对应的jdbcType名称,用于sql设置值.<br>
   * 默认使用{@link Column#columnDefinition()}内的列定义,如果为空则使用默认值.
   *
   * @param field the field
   * @param defaultValue 默认值
   * @return jdbcType名称
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
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    IFunction<AbstractEntityPoJo, Date> function = AbstractEntityPoJo::getCreateTime;
    log.info("main:[{}]", getColumn(function));
  }
}
