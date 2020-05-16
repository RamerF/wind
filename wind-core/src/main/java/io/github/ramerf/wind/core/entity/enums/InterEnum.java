package io.github.ramerf.wind.core.entity.enums;

import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.EnumUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.Serializable;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** The interface Inter enum. */
public interface InterEnum extends Serializable {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(InterEnum.class);

  /**
   * Value integer.
   *
   * @return the integer
   */
  Integer value();

  /**
   * Desc string.
   *
   * @return the string
   */
  String desc();

  /**
   * Gets value.
   *
   * @return the value
   */
  default Integer getValue() {
    return value();
  }

  /**
   * 通过枚举值获取枚举实例
   *
   * @param <T> the type parameter
   * @param enumType 枚举类
   * @param value 枚举值
   * @param allowNull 是否允许获取到的枚举实例为null<br>
   *     设置为true时,如果枚举实例为null,将会抛出{@link CommonException}
   * @return 枚举实例 {@link InterEnum}
   * @throws CommonException the {@link CommonException}
   */
  static <T extends InterEnum> T of(Class<T> enumType, final Integer value, final boolean allowNull)
      throws CommonException {
    final T t = EnumUtils.of(enumType, value);
    if (Objects.isNull(t) && !allowNull) {
      throw CommonException.of(
          ResultCode.API_PARAM_INVALID.desc(StringUtils.firstLowercase(enumType.getSimpleName())));
    }
    return t;
  }

  /**
   * Of t.
   *
   * @param <T> the type parameter
   * @param enumType the enum type
   * @param value the value
   * @return the t
   * @see InterEnum#of(Class, Integer, boolean) InterEnum#of(Class, Integer, boolean)
   */
  static <T extends InterEnum> T of(Class<T> enumType, final Integer value) {
    return of(enumType, value, true);
  }
}
