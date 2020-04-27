package io.github.ramerf.mybatisturbo.core.entity.enums;

import com.baomidou.mybatisplus.core.enums.IEnum;
import io.github.ramerf.mybatisturbo.core.entity.response.ResultCode;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.util.EnumUtils;
import io.github.ramerf.mybatisturbo.core.util.StringUtils;
import java.io.Serializable;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public interface InterEnum extends IEnum<Integer>, Serializable {
  Logger log = LoggerFactory.getLogger(InterEnum.class);

  Integer value();

  String desc();

  @Override
  default Integer getValue() {
    return value();
  }

  /**
   * 通过枚举值获取枚举实例
   *
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

  /** @see InterEnum#of(Class, Integer, boolean) */
  static <T extends InterEnum> T of(Class<T> enumType, final Integer value) {
    return of(enumType, value, true);
  }
}
