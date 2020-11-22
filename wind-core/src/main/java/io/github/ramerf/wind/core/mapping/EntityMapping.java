package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nonnull;
import lombok.Data;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toList;

/**
 * 保存实体关联关系.
 *
 * @author ramer
 * @since 19/09/2020
 */
@Data
public class EntityMapping<T extends AbstractEntityPoJo<T, ?>> {
  /** {@link Collections#singletonList(Object)}. */
  private static Map<Class<?>, List<MappingInfo>> ENTITY_MAPPING = new ConcurrentHashMap<>();

  public static <T extends AbstractEntityPoJo<T, ?>> List<MappingInfo> get(
      @Nonnull Class<T> clazz) {
    return ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  public static Optional<MappingInfo> get(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz, final Field field) {
    final List<MappingInfo> infos = ENTITY_MAPPING.get(getCglibProxyTarget(clazz));
    return infos.stream().filter(o -> o.field.equals(field)).findFirst();
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  public static Optional<MappingInfo> get(
      @Nonnull Class<? extends AbstractEntityPoJo> clazz,
      final Class<? extends AbstractEntityPoJo> referenceClazz) {
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

  public static <T extends AbstractEntityPoJo<T, ?>> void initial(final EntityInfo entityInfo) {
    @SuppressWarnings("unchecked")
    final Class<T> clazz = (Class<T>) entityInfo.getClazz();
    final List<MappingInfo> mappingInfos =
        BeanUtils.retrievePrivateFields(clazz, ArrayList::new).stream()
            .filter(field -> MappingInfo.isOneMapping(field) || MappingInfo.isManyMapping(field))
            .map(MappingInfo::of)
            .collect(toList());
    put(clazz, mappingInfos);
    entityInfo.setMappingInfos(mappingInfos);
  }

  public static <T extends AbstractEntityPoJo<T, ?>, E extends AbstractEntityPoJo<E, ?>> void valid(
      @Nonnull final Map<Class<?>, EntityInfo> map) {
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
                          @SuppressWarnings("unchecked")
                          final Class<E> referenceClazz =
                              (Class<E>) mappingInfo.getReferenceClazz();
                          final EntityInfo referenceEntityInfo = map.get(referenceClazz);
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
    @SuppressWarnings("rawtypes")
    private Class<? extends AbstractEntityPoJo> clazz;
    /** 当前对象的列. */
    private Field field;

    /** 当前对象的列. */
    private String column;

    /** 关联对象的列. */
    private String referenceColumn;

    /** 关联对象的字段. */
    private Field referenceField;

    /** 关联对象. */
    @SuppressWarnings("rawtypes")
    private Class<? extends AbstractEntityPoJo> referenceClazz;

    /** 引用定义.预留字段. */
    private String referenceDdlDefinition;

    private MappingType mappingType;

    private MappingInfo() {}

    @SuppressWarnings("unchecked")
    private static <T extends AbstractEntityPoJo<T, ?>, E extends AbstractEntityPoJo<E, ?>>
        MappingInfo of(final Field field) {
      final MappingInfo mappingInfo = new MappingInfo();
      mappingInfo.setClazz((Class<T>) field.getDeclaringClass());
      mappingInfo.setField(field);
      mappingInfo.setMappingType(MappingType.of(field));
      mappingInfo.setColumn(EntityUtils.fieldToColumn(field));
      if (isManyMapping(field)) {
        final Type type =
            ((ParameterizedTypeImpl) field.getGenericType()).getActualTypeArguments()[0];
        mappingInfo.setReferenceClazz((Class<E>) type);
        return mappingInfo;
      }

      mappingInfo.setReferenceClazz((Class<E>) field.getType());
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
        return AbstractEntityPoJo.class.isAssignableFrom(
            (Class<?>)
                ((ParameterizedTypeImpl) field.getGenericType()).getActualTypeArguments()[0]);
      }
      return false;
    }

    /** 是否是N对1映射.true:是 */
    public static boolean isOneMapping(final Field field) {
      return AbstractEntityPoJo.class.isAssignableFrom(field.getType())
          && (field.getAnnotation(OneToOne.class) != null
              || field.getAnnotation(ManyToOne.class) != null);
    }

    /** 是否是有效关系映射.true:是 */
    public static boolean isValidMapping(final Field field) {
      return isOneMapping(field) || isManyMapping(field);
    }
  }

  /** 获取CGLIB代理目标对象,截取(0,$$)之间的字符. */
  public static <T extends AbstractEntityPoJo<T, ?>> Class<T> getCglibProxyTarget(
      final Class<T> clazz) {
    String name = clazz.getName();
    final int index = name.indexOf("$$");
    return BeanUtils.getClazz(index != -1 ? name.substring(0, index) : name);
  }
}
