package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
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
  /** {@link Collections#singletonList(Object)}. */
  private static Map<Class<? extends AbstractEntityPoJo>, List<MappingInfo>> ENTITY_MAPPING =
      new ConcurrentHashMap<>();

  public static List<MappingInfo> get(@Nonnull Class<? extends AbstractEntityPoJo> clazz) {
    return ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
  }

  public static Optional<MappingInfo> get(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz, final Field field) {
    return ENTITY_MAPPING.get(getCglibProxyTarget(clazz)).stream()
        .filter(o -> o.field.equals(field))
        .findFirst();
  }

  public static void put(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz, @Nonnull final MappingInfo mappingInfo) {
    ENTITY_MAPPING.merge(
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
    ENTITY_MAPPING.put(clazz, mappingInfos);
  }

  public static void initial(final EntityInfo entityInfo) {
    @SuppressWarnings("unchecked")
    final Class<AbstractEntityPoJo> clazz = (Class<AbstractEntityPoJo>) entityInfo.getClazz();
    final List<MappingInfo> mappingInfos =
        BeanUtils.retrievePrivateFields(clazz, ArrayList::new).stream()
            // TODO-WARN 可能类型是集合
            .filter(field -> AbstractEntityPoJo.class.isAssignableFrom(field.getType()))
            .map(MappingInfo::of)
            .collect(Collectors.toList());
    put(clazz, mappingInfos);
    entityInfo.setMappingInfos(mappingInfos);
  }

  public static void valid(@Nonnull final Map<String, EntityInfo> map) {
    if (map.size() == 0) {
      return;
    }
    map.values()
        .forEach(
            entityInfo ->
                entityInfo
                    .getMappingInfos()
                    .forEach(
                        mappingInfo -> {
                          final Class<? extends AbstractEntityPoJo> referenceClazz =
                              mappingInfo.getReferenceClazz();
                          final EntityInfo referenceEntityInfo =
                              map.get(referenceClazz.getTypeName());
                          if (referenceEntityInfo == null) {
                            throw CommonException.of(
                                String.format(
                                    "The %s reference object %s is not a managed entity.",
                                    entityInfo.getClazz().getName(), referenceClazz.getName()));
                          }
                          /*
                           * 这里是一对多的关系,可能是对象和对象的id两个字段,但它们都映射到同一列.
                           * 如:
                           * <pre>
                           * private Foo foo;
                           * private Long fooId;
                           */
                          final Optional<EntityColumn> optional =
                              referenceEntityInfo.getEntityColumns().stream()
                                  .filter(
                                      column ->
                                          column.getName().equals(mappingInfo.getReferenceColumn()))
                                  .findFirst();
                          if (!optional.isPresent()) {
                            throw CommonException.of(
                                String.format(
                                    "The %s reference column %s, but not found in %s",
                                    entityInfo.getClazz().getName(),
                                    mappingInfo.getReferenceColumn(),
                                    referenceClazz.getName()));
                          }
                          // 更新关联关系
                          mappingInfo.setReferenceField(optional.get().getField());
                        }));
  }

  @Data
  public static class MappingInfo {
    /** 当前对象的列. */
    private Field field;

    /** 当前对象的列. */
    private String column;

    /** 关联对象的列. */
    private String referenceColumn;

    /** 关联对象的字段. */
    private Field referenceField;

    /** 关联对象. */
    private Class<? extends AbstractEntityPoJo> referenceClazz;

    /** 引用定义.预留字段. */
    private String referenceDdlDefinition;

    private MappingType mappingType;

    private MappingInfo() {}

    @SuppressWarnings("unchecked")
    private static MappingInfo of(final Field field) {
      final MappingInfo info = new MappingInfo();
      // TODO-WARN 要根据@OneToOne && @JoinColumn获取
      info.setField(field);
      info.setMappingType(MappingType.of(field));
      info.setColumn(EntityUtils.fieldToColumn(field));
      info.setReferenceClazz((Class<? extends AbstractEntityPoJo>) field.getType());
      info.setReferenceColumn(getReferencedColumn(field));
      return info;
    }

    private static String getReferencedColumn(final Field field) {
      final JoinColumn joinColumn = field.getAnnotation(JoinColumn.class);
      if (joinColumn != null && StringUtils.nonEmpty(joinColumn.referencedColumnName())) {
        return joinColumn.referencedColumnName();
      }
      // 默认关联id
      return "id";
    }
  }

  /** 获取CGLIB代理目标对象,截取(0,$$)之间的字符. */
  public static <T extends AbstractEntityPoJo> Class<T> getCglibProxyTarget(final Class<T> clazz) {
    String name = clazz.getName();
    final int index = name.indexOf("$$");
    return BeanUtils.getClazz(index != -1 ? name.substring(0, index) : name);
  }
}
