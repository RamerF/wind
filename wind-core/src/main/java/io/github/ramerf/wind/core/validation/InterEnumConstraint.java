package io.github.ramerf.wind.core.validation;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.lang.annotation.*;
import javax.validation.Constraint;
import javax.validation.Payload;

/**
 * 该注解用于校验枚举.<br>
 * 用法：<br>
 * {@code @InterEnumConstraint(message = "状态不正确", clazz = Status.class)}<br>
 * private int status;
 *
 * @see InterEnum
 */
@Documented
@Constraint(validatedBy = InterEnumValidator.class)
@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface InterEnumConstraint {
  /**
   * Message string.
   *
   * @return the string
   */
  String message();

  /** 枚举类. @return the class @return the class @return the class */
  @SuppressWarnings("rawtypes")
  Class<? extends InterEnum> clazz();

  /** 是否必传.默认非必传 */
  boolean required() default false;

  /**
   * Groups class [ ].
   *
   * @return the class [ ]
   */
  Class<?>[] groups() default {};

  /**
   * Payload class [ ].
   *
   * @return the class [ ]
   */
  Class<? extends Payload>[] payload() default {};
}
