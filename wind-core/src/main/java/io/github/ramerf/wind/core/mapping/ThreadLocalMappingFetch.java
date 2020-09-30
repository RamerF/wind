package io.github.ramerf.wind.core.mapping;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import java.util.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Thread local mapping fetch.
 *
 * @since 2020.09.27
 * @author Tang Xiaofeng
 */
@Slf4j
public class ThreadLocalMappingFetch {
  /**
   * 当前线程对应的关联查询栈,用于解决循环关联.<br>
   * 每次执行关联查询时拼接到对应的调用链
   */
  private static final ThreadLocal<List<String>> localChain =
      ThreadLocal.withInitial(ArrayList::new);

  /** 每个线程相同的对象只会查询一次. */
  private static final ThreadLocal<List<LocalFetchObject>> localFetchObject =
      ThreadLocal.withInitial(ArrayList::new);

  private static final ThreadLocal<Map<CachedObject, Object>> localCachedObject =
      ThreadLocal.withInitial(HashMap::new);

  /**
   * Add.
   *
   * @param poJo the po jo
   * @param mappingInfo the mappingInfo
   * @param relationValue the relation value
   */
  public static void add(
      final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
    final String fetchChain = getFetchChain(mappingInfo, relationValue);
    final List<String> chains = localChain.get();
    if (chains.size() == 0) {
      chains.add(fetchChain);
      return;
    }
    for (int i = 0; i < chains.size(); i++) {
      final String chain = chains.get(i);
      if (chain.endsWith(fetchChain)) {
        chains.set(i, chain.concat(" -> ").concat(fetchChain));
        break;
      }
      // 新增
      if (i == chains.size() - 1) {
        chains.add(fetchChain);
      }
    }
    log.debug("add:[{}]", localChain.get());
  }

  private static String getFetchChain(final MappingInfo mappingInfo, final Object relationValue) {
    final String className = mappingInfo.getField().getType().getName();
    return className + "-" + relationValue;
  }

  /** Clear. */
  public static void clear() {
    localChain.remove();
    localFetchObject.remove();
    localCachedObject.remove();
  }

  /**
   * 是否是循环调用.关联查询相同对象将执行第二次时返回true.
   *
   * @param mappingInfo the mapping info
   * @param relationValue the relation value
   * @return the boolean
   */
  public static boolean loopFetch(final MappingInfo mappingInfo, final Object relationValue) {
    final String fetchChain = getFetchChain(mappingInfo, relationValue);
    return localChain.get().stream().anyMatch(str -> str.startsWith(fetchChain));
  }

  @SuppressWarnings("unchecked")
  public static <T> T getFetchObject(
      final AbstractEntityPoJo poJo, final MappingInfo mappingInfo, final Object relationValue) {
    return (T)
        localFetchObject.get().stream()
            .filter(o -> o.equals(LocalFetchObject.of(poJo, mappingInfo, relationValue, null)))
            .findFirst()
            .map(LocalFetchObject::getFetchObject)
            .orElse(null);
  }

  static class LocalFetchObject {
    private AbstractEntityPoJo poJo;
    private MappingInfo mappingInfo;
    private Object relationValue;
    @Getter private Object fetchObject;

    public static LocalFetchObject of(
        final AbstractEntityPoJo poJo,
        final MappingInfo mappingInfo,
        final Object relationValue,
        final Object fetchObject) {
      final LocalFetchObject object = new LocalFetchObject();
      object.poJo = poJo;
      object.mappingInfo = mappingInfo;
      object.relationValue = relationValue;
      object.fetchObject = fetchObject;
      return object;
    }

    @Override
    public boolean equals(final Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      final LocalFetchObject object = (LocalFetchObject) o;
      return poJo.equals(object.poJo)
          && mappingInfo.equals(object.mappingInfo)
          && relationValue.equals(object.relationValue);
    }

    @Override
    public int hashCode() {
      return Objects.hash(poJo, mappingInfo, relationValue);
    }
  }

  public static void putObject(final AbstractEntityPoJo object, final Object relationValue) {
    final Class<? extends AbstractEntityPoJo> target =
        EntityMapping.getCglibProxyTarget(object.getClass());
    log.info("putObject:[{}]", target.getName());
    localCachedObject.get().put(CachedObject.of(target, relationValue), object);
  }

  @SuppressWarnings("unchecked")
  public static <T extends AbstractEntityPoJo> T getObject(
      final Class<? extends AbstractEntityPoJo> clazz, final Object id) {
    return (T) localCachedObject.get().get(CachedObject.of(clazz, id));
  }

  public static boolean isCached(final Class<? extends AbstractEntityPoJo> clazz, final Object id) {
    return localCachedObject.get().containsKey(CachedObject.of(clazz, id));
  }

  public static class CachedObject {
    private Class<? extends AbstractEntityPoJo> clazz;
    private Object id;

    public static CachedObject of(
        final Class<? extends AbstractEntityPoJo> clazz, final Object id) {
      final CachedObject object = new CachedObject();
      object.clazz = clazz;
      object.id = id;
      return object;
    }
  }
}
