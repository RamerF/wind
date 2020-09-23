package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import javax.persistence.JoinColumn;
import lombok.Data;

/**
 * 保存实体关联关系.
 *
 * @author ramer
 * @since 19/09/2020
 */
@Data
public class EntityMapping {
  /** List不支持更新操作. */
  private static Map<Class<? extends AbstractEntityPoJo>, List<MappingInfo>> mapping =
      new ConcurrentHashMap<>();

  public static List<MappingInfo> get(@Nonnull Class<? extends AbstractEntityPoJo> clazz) {
    return mapping.get(clazz);
  }

  public static Optional<MappingInfo> get(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz, final Field field) {
    return mapping.get(clazz).stream().filter(o -> o.field.equals(field)).findFirst();
  }

  public static void put(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz, @Nonnull final MappingInfo mappingInfo) {
    mapping.merge(
        clazz,
        Collections.singletonList(mappingInfo),
        (old, newVal) -> {
          old.addAll(newVal);
          return old;
        });
  }

  public static void put(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz,
      @Nonnull final List<MappingInfo> mappingInfos) {
    mapping.put(clazz, mappingInfos);
  }

  public static void initial(final EntityInfo entityInfo) {
    @SuppressWarnings("unchecked")
    final Class<AbstractEntityPoJo> clazz = (Class<AbstractEntityPoJo>) entityInfo.getClazz();
    put(
        clazz,
        BeanUtils.retrievePrivateFields(clazz, ArrayList::new).stream()
            .filter(field -> AbstractEntityPoJo.class.isAssignableFrom(field.getType()))
            .map(MappingInfo::of)
            .collect(Collectors.toList()));
  }

  @Data
  public static class MappingInfo {
    private Class<?> clazz;
    private Field field;

    private String key;

    private MappingType mappingType;

    private MappingInfo() {}

    public static MappingInfo of(final Field field) {
      final MappingInfo info = new MappingInfo();
      info.setClazz(field.getType());
      info.setField(field);
      info.setMappingType(MappingType.of(field));
      info.setKey(getMappingKey(field));
      return info;
    }

    private static String getMappingKey(final Field field) {
      final JoinColumn joinColumn = field.getAnnotation(JoinColumn.class);
      if (joinColumn != null) {
        return joinColumn.referencedColumnName();
      }
      return EntityUtils.fieldToColumn(field);
    }
  }
}
