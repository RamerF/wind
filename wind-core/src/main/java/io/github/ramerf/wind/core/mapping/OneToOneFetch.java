package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.handler.BeanResultHandler;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.util.Optional;
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
public class OneToOneFetch {
  /** The Po jo. */
  @Getter final AbstractEntityPoJo poJo;

  /** The Field. */
  @Getter final MappingInfo mappingInfo;

  /**
   * Instantiates a new One to one fetch.
   *
   * @param poJo the po jo
   * @param mappingInfo the mappingInfo
   */
  public OneToOneFetch(final AbstractEntityPoJo poJo, final MappingInfo mappingInfo) {
    this.poJo = poJo;
    this.mappingInfo = mappingInfo;
  }

  /**
   * Gets fetch proxy.
   *
   * @return the fetch proxy
   */
  public Object getFetchProxy() {
    return Enhancer.create(mappingInfo.getReferenceClazz(), new OneToOneLazyLoader(this));
  }

  /**
   * The type One to one lazy loader.
   *
   * @since 2020.09.26
   * @author Tang Xiaofeng
   */
  @Slf4j
  public static class OneToOneLazyLoader extends AbstractLazyLoader {
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
      final Optional<MappingInfo> optional =
          EntityMapping.get(poJo.getClass(), mappingInfo.getReferenceField());
      if (!optional.isPresent()) {
        return null;
      }

      final MappingInfo mappingInfo = optional.get();
      final Object value =
          mappingInfo.getReferenceColumn().equals("id")
              ? poJo.getId()
              : BeanUtils.getValue(poJo, mappingInfo.getField(), null);
      @SuppressWarnings("unchecked")
      final Class<AbstractEntityPoJo> clazz =
          (Class<AbstractEntityPoJo>) mappingInfo.getReferenceClazz();
      final BeanResultHandler<AbstractEntityPoJo> handler = new BeanResultHandler<>(clazz, null);
      handler.setProxy(false);
      @SuppressWarnings("unchecked")
      final Object mapping =
          Query.getInstance()
              .select(fromClass(clazz))
              .stringWhere(condition -> condition.eq(mappingInfo, value))
              .fetchOne(clazz, handler);
      return mapping;
    }
  }
}
