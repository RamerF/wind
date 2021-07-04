package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.Data;

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

  public static Optional<MappingInfo> get(@Nonnull Class<?> clazz, final Field field) {
    final List<MappingInfo> infos = ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
    return infos.stream().filter(o -> o.field.equals(field)).findFirst();
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
            .map(MappingType::getMappingInfo)
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
                          if (mappingInfo.targetColumn == null) {
                            return;
                          }
                          final Class<?> referenceClazz = mappingInfo.getTargetClazz();
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
                                          column.getName().equals(mappingInfo.getTargetColumn()))
                                  .findFirst();
                          // throw new IllegalStateException(
                          //     String.format(
                          //         "The %s reference column [%s], but not found in %s",
                          //         entityInfo.getClazz().getName(),
                          //         mappingInfo.getReferenceColumn(),
                          //         referenceClazz.getName()));
                          // 更新关联关系
                          optional.ifPresent(
                              column -> mappingInfo.setTargetField(column.getField()));
                        }));
  }

  @Data
  public static class MappingInfo {
    /* 示例 Many多对一关联 One.Many表中添加one_id字段关联One表id列,One的关联关系. */
    /** 当前对象.如:Many.class */
    private Class<?> clazz;
    /** 当前对象关联对方的字段.如:<code>private One one;</code> */
    private Field field;
    /** 当前对象新增列. */
    private Field joinField;

    /** 当前对象添加的列.如:one_id */
    private String joinColumn;

    /** 关联对象.如:One.class */
    private Class<?> targetClazz;

    /** 关联对象的字段,n对多时为空.如:<code>private Long id;</code> */
    private Field targetField;

    /** 关联对象的列,n对多时为空.如:id */
    private String targetColumn;

    /** 引用定义.预留字段. */
    private String targetDdlDefinition;

    private MappingType mappingType;

    protected MappingInfo() {}

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

    public <T> T getMappingObject(final Object object) {
      return this.mappingType.fetchMapping(object, this);
    }

    @SuppressWarnings("unchecked")
    public <T> Class<T> getTargetClazz() {
      return (Class<T>) targetClazz;
    }
  }

  /** 获取CGLIB代理目标对象,截取(0,$$)之间的字符. */
  public static Class<?> getCglibProxyTarget(final Class<?> clazz) {
    String name = clazz.getName();
    final int index = name.indexOf("$$");
    return BeanUtils.getClazz(index != -1 ? name.substring(0, index) : name);
  }
}
