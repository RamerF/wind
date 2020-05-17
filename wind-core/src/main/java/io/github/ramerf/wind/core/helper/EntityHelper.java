package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.persistence.Column;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
@Slf4j
public class EntityHelper {
  private static final Map<String, Map<String, String>> FIELD_COLUMN_MAP =
      new ConcurrentHashMap<>();

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

  public static String getColumn(BeanFunction function) {
    if (log.isDebugEnabled()) {
      log.debug("getColumn:[{}]", FIELD_COLUMN_MAP);
    }
    return FIELD_COLUMN_MAP
        .get(LambdaUtils.getActualTypePath(function))
        .get(BeanUtils.methodToProperty(LambdaUtils.getMethodName(function)));
  }

  public static void main(String[] args) {
    initEntity(AbstractEntityPoJo.class);
    IFunction<AbstractEntityPoJo, Long> function = AbstractEntityPoJo::getCreateId;
    log.info("main:[{}]", getColumn(function));
  }
}
