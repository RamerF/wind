package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.*;
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

  public static <T extends AbstractEntity> void initEntity(final Class<T> clazz) {
    final Field[] fields = clazz.getDeclaredFields();
    Map<String, String> map = new HashMap<>(10);
    Arrays.stream(fields)
        .filter(field -> !Modifier.isStatic(field.getModifiers()))
        .filter(field -> !Modifier.isTransient(field.getModifiers()))
        .filter(field -> !Modifier.isPublic(field.getModifiers()))
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

  public static <T extends AbstractEntity> String getColumn(IFunction<T, ?> function) {
    if(FIELD_COLUMN_MAP.size() == 0){
//      BeanUtils.scanClasses("", AbstractEntity.class);
    }
    return FIELD_COLUMN_MAP
        .get(LambdaUtils.getActualTypePath(function))
        .get(BeanUtils.methodToProperty(LambdaUtils.getMethodName(function)));
  }

  public static void main(String[] args) {
    initEntity(AbstractEntityPoJo.class);
    log.info("main:[{}]", getColumn(AbstractEntityPoJo::getCompanyId));
  }
}
