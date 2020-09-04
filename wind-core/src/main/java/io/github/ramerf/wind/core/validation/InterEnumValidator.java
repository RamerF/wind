package io.github.ramerf.wind.core.validation;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.internal.engine.constraintvalidation.ConstraintValidatorContextImpl;
import org.springframework.stereotype.Component;

/**
 * 校验{@link InterEnum}子类.
 *
 * @since 2020.08.31
 * @author Tang Xiaofeng
 */
@Slf4j
@Component
@SuppressWarnings("rawtypes")
public class InterEnumValidator implements ConstraintValidator<InterEnumConstraint, InterEnum> {

  @Override
  public void initialize(InterEnumConstraint constraint) {}

  @Override
  public boolean isValid(InterEnum value, ConstraintValidatorContext cxt) {
    ConstraintValidatorContextImpl context = (ConstraintValidatorContextImpl) cxt;
    final InterEnumConstraint annotation =
        (InterEnumConstraint) context.getConstraintDescriptor().getAnnotation();
    return !annotation.required() || value != null;
  }
}
