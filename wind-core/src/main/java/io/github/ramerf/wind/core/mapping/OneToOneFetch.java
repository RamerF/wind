package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.handler.BeanResultHandler;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.util.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cglib.proxy.Enhancer;

import static io.github.ramerf.wind.core.factory.QueryColumnFactory.fromClass;

/**
 * The type One to one fetch.
 *
 * @since 2020.09.26
 * @author Tang Xiaofeng
 */
@Slf4j
public class OneToOneFetch {
  /** The Po jo. */
  @Getter final AbstractEntityPoJo poJo;

  /** The Field. */
  @Getter final MappingInfo mappingInfo;

  @Getter final Object relationValue;

  /**
   * Instantiates a new One to one fetch.
   *
   * @param poJo the po jo
   * @param mappingInfo the mappingInfo
   * @param relationValue the relation value
   * @since 2020.09.28
   * @author Tang Xiaofeng
   */
  public OneToOneFetch(
      final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
    this.poJo = poJo;
    this.mappingInfo = mappingInfo;
    this.relationValue = relationValue;
  }

  /**
   * Gets fetch proxy.
   *
   * @return the fetch proxy
   */
  public <T extends AbstractEntityPoJo> T getFetchProxy() {
    // if (ThreadLocalMappingFetch.loopFetch(mappingInfo, relationValue)) {
    //   ThreadLocalMappingFetch.clear();
    //   return null;
    // }
    return (T) Enhancer.create(mappingInfo.getReferenceClazz(), new OneToOneLazyLoader(this));
  }

  /**
   * The type One to one lazy loader.
   *
   * @since 2020.09.26
   * @author Tang Xiaofeng
   */
  @Slf4j
  public static class OneToOneLazyLoader extends AbstractLazyLoader {
    private static final Map<Class<? extends AbstractEntityPoJo>, Object> EMPTY_POJO =
        new WeakHashMap<>();

    /**
     * Instantiates a new One to one lazy loader.
     *
     * @param fetch the fetch
     */
    public OneToOneLazyLoader(final OneToOneFetch fetch) {
      super(fetch);
    }

    @Override
    public Object loadObject() {
      // ThreadLocalMappingFetch.add(poJo, mappingInfo, relationValue);
      @SuppressWarnings("unchecked")
      final Class<AbstractEntityPoJo> clazz =
          (Class<AbstractEntityPoJo>) mappingInfo.getReferenceClazz();
      final Optional<MappingInfo> optional =
          EntityMapping.get(poJo.getClass(), mappingInfo.getField());
      if (!optional.isPresent()) {
        return getEmptyPoJo(clazz);
      }
      final MappingInfo mappingInfo = optional.get();
      final Object value =
          mappingInfo.getReferenceColumn().equals("id")
              ? relationValue
              : BeanUtils.getValue(poJo, mappingInfo.getField(), null);
      final QueryColumn<AbstractEntityPoJo> queryColumn = fromClass(clazz);
      final BeanResultHandler<AbstractEntityPoJo> handler =
          new BeanResultHandler<>(clazz, false, queryColumn);
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(queryColumn)
              .stringWhere(condition -> condition.eq(mappingInfo, value))
              .fetchOne(clazz, handler);
      return mapping == null ? getEmptyPoJo(clazz) : mapping;
    }

    public static <T extends AbstractEntityPoJo> T getEmptyPoJo(
        Class<? extends AbstractEntityPoJo> clazz) {
      //noinspection unchecked
      return (T) EMPTY_POJO.putIfAbsent(clazz, BeanUtils.initial(clazz));
    }
  }
}