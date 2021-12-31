package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.exception.CommonException;

/**
 * id生成策略.
 *
 * @author ramer
 * @since 2020 /5/20
 */
@FunctionalInterface
public interface IdGenerator {
  /** 自增 */
  IdGenerator AUTO_INCREMENT_ID_GENERATOR = new AutoIncrementIdGenerator();

  /** 零值,该值表示用户未指定 */
  IdGenerator VOID_ID_GENERATOR = VoidIdGenerator.INSTANCE;

  /**
   * 生成id.
   *
   * @param obj 当前对象
   * @return the long
   */
  Object nextId(final Object obj);

  class AutoIncrementIdGenerator implements IdGenerator {

    @Override
    public Object nextId(Object obj) {
      return null;
    }
  }

  class VoidIdGenerator implements IdGenerator {
    private static final VoidIdGenerator INSTANCE = new VoidIdGenerator();

    private VoidIdGenerator() {}

    @Override
    public Object nextId(Object obj) {
      throw new CommonException("Void id generator");
    }
  }
}
