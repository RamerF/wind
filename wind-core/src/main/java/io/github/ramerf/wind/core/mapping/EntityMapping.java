package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.Data;

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toList;

/**
 * 保存实体关联关系.
 *
 * @author ramer
 * @since 19/09/2020
 */
@Data
public class EntityMapping {
  /** {@link Collections#singletonList(Object)}. */
  private static Map<Class<?>, List<MappingInfo>> ENTITY_MAPPING = new ConcurrentHashMap<>();

  public static List<MappingInfo> get(@Nonnull Class<?> clazz) {
    return ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
  }

  public static Optional<MappingInfo> get(@Nonnull Class<?> clazz, final Field field) {
    final List<MappingInfo> infos = ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
    return infos.stream().filter(o -> o.field.equals(field)).findFirst();
  }

  public static Optional<MappingInfo> get(@Nonnull Class<?> clazz, final Class<?> referenceClazz) {
    final List<MappingInfo> infos = ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
    return infos.stream().filter(o -> o.referenceClazz.equals(referenceClazz)).findFirst();
  }

  public static void put(@Nonnull Class<?> clazz, @Nonnull final MappingInfo mappingInfo) {
    ENTITY_MAPPING.merge(
        clazz,
        Collections.singletonList(mappingInfo),
        (old, newVal) -> {
          old.addAll(newVal);
          return old;
        });
  }

  public static void put(@Nonnull Class<?> clazz, @Nonnull final List<MappingInfo> mappingInfos) {
    ENTITY_MAPPING.put(clazz, mappingInfos);
  }

  public static void initial(final EntityInfo entityInfo) {
    final Class<?> clazz = entityInfo.getClazz();
    final List<MappingInfo> mappingInfos =
        BeanUtils.retrievePrivateFields(clazz, ArrayList::new).stream()
            .filter(MappingInfo::isValidMapping)
            .map(MappingInfo::of)
            .collect(toList());
    put(clazz, mappingInfos);
    entityInfo.setMappingInfos(mappingInfos);
  }

  public static void valid(@Nonnull final Map<Class<?>, EntityInfo> map) {
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
                          if (mappingInfo.referenceColumn == null) {
                            return;
                          }
                          final Class<?> referenceClazz = mappingInfo.getReferenceClazz();
                          final EntityInfo referenceEntityInfo = map.get(referenceClazz);
                          if (referenceEntityInfo == null) {
                            throw new IllegalStateException(
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
                            throw new IllegalStateException(
                                String.format(
                                    "The %s reference column [%s], but not found in %s",
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
    private Class<?> clazz;
    /** 当前对象的列. */
    private Field field;

    /** 当前对象的列. */
    private String column;

    /** 关联对象的列. */
    private String referenceColumn;

    /** 关联对象的字段. */
    private Field referenceField;

    /** 关联对象. */
    private Class<?> referenceClazz;

    /** 引用定义.预留字段. */
    private String referenceDdlDefinition;

    private MappingType mappingType;

    private MappingInfo() {}

    @SuppressWarnings("unchecked")
    private static <T, E> MappingInfo of(final Field field) {
      final MappingInfo mappingInfo = new MappingInfo();
      mappingInfo.setClazz(field.getDeclaringClass());
      mappingInfo.setField(field);
      mappingInfo.setMappingType(MappingType.of(field));
      mappingInfo.setColumn(EntityUtils.fieldToColumn(field));
      if (isManyMapping(field)) {
        final Type type = ((ParameterizedType) field.getGenericType()).getActualTypeArguments()[0];
        mappingInfo.setReferenceClazz((Class<E>) type);
        return mappingInfo;
      }

      mappingInfo.setReferenceClazz(field.getType());
      final String joinColumnName;
      final String reference;
      final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
      if (oneToOne != null) {
        joinColumnName = oneToOne.joinColumnName();
        reference = oneToOne.referenceField();
      } else {
        final ManyToOne manyToOne = field.getAnnotation(ManyToOne.class);
        joinColumnName = manyToOne.joinColumnName();
        reference = manyToOne.referenceField();
      }
      // 手动指定关联列名
      if (!"".equals(joinColumnName)) mappingInfo.setReferenceColumn(joinColumnName);
      else mappingInfo.setReferenceColumn(camelToUnderline(reference));
      return mappingInfo;
    }

    /** 是否是1对N/N对N映射.true:是 */
    public static boolean isManyMapping(final Field field) {
      // || field.getAnnotation(ManyToMany.class) != null
      final Class<?> type = field.getType();
      if ((List.class.isAssignableFrom(type) || Set.class.isAssignableFrom(type))
          && (field.getAnnotation(OneToMany.class) != null)) {
        return ((Class<?>) ((ParameterizedType) field.getGenericType()).getActualTypeArguments()[0])
            .isAnnotationPresent(TableInfo.class);
      }
      return false;
    }

    /** 是否是N对1映射.true:是 */
    public static boolean isOneMapping(final Field field) {
      return field.getType().isAnnotationPresent(TableInfo.class)
          && (field.getAnnotation(OneToOne.class) != null
              || field.getAnnotation(ManyToOne.class) != null);
    }

    /** 是否是有效关系映射.true:是 */
    public static boolean isValidMapping(final Field field) {
      return isOneMapping(field) || isManyMapping(field);
    }

    /** 获取默认关联的主键. */
    private static String getMappingPrimaryKey(final Field field) {

      return StringUtils.camelToUnderline(field.getName());
    }
  }

  /** 获取CGLIB代理目标对象,截取(0,$$)之间的字符. */
  public static Class<?> getCglibProxyTarget(final Class<?> clazz) {
    String name = clazz.getName();
    final int index = name.indexOf("$$");
    return BeanUtils.getClazz(index != -1 ? name.substring(0, index) : name);
  }
}
