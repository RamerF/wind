package io.github.ramerf.wind.core.validation;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.*;
import javax.annotation.Nonnull;
import javax.validation.ConstraintViolation;
import javax.validation.Validator;
import javax.validation.groups.Default;
import lombok.Getter;
import lombok.NonNull;
import org.hibernate.validator.internal.engine.path.PathImpl;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;
import org.springframework.validation.*;

/**
 * 校验工具类.
 *
 * <pre>
 * 用法一: 手动校验<br>
 * ViolationResult violationResult = ValidateUtil.validate(list);
 *  if (violationResult.hasErrors()) {
 *    return Result.error(ValidateUtil.collect(violationResult));
 * }
 *
 * 用法二: @Valid自动校验<br>
 * {@code @PostMapping}
 * public Rs foo(@Valid Foo foo, BindingResult bindingResult) {
 *  if (bindingResult.hasErrors()) {
 *    return Result.error(ValidateUtil.collectFirst(bindingResult));
 *  }
 *  return Result.success();
 * }
 *
 * </pre>
 *
 * @author Tang Xiaofeng
 * @since 2020.08.31
 */
@Component
public final class ValidateUtil implements ApplicationContextAware {
  private static Validator validator;

  @Override
  public void setApplicationContext(final ApplicationContext applicationContext)
      throws BeansException {
    ValidateUtil.validator = applicationContext.getBean(Validator.class);
  }

  /**
   * 校验集合.<b>会在第一个对象校验失败时停止校验</b>.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param groups the group or list of groups targeted for validation (defaults to * {@link
   *     Default})
   * @return the violation result
   */
  public static <T> ViolationResult validate(final List<T> ts, Class<?>... groups) {
    if (ts.size() < 1) {
      return ViolationResult.EMPTY;
    }
    final ViolationResult violationResult = new ViolationResult();
    for (T t : ts) {
      final ViolationResult result = validate(t, groups);
      violationResult.addError(result.getViolationErrors());
      // 第一个校验失败就停止
      if (result.hasErrors()) {
        break;
      }
    }
    return violationResult;
  }

  private static <T> ViolationResult validate(T t, Class<?>... groups) {
    final Set<ConstraintViolation<T>> violations = validator.validate(t, groups);
    ViolationResult result = new ViolationResult();
    violations.forEach(
        violation -> {
          final String path = ((PathImpl) violation.getPropertyPath()).getLeafNode().getName();
          final String message = violation.getMessage();
          result.addError(ViolationErrors.of(path, message));
        });
    return result;
  }

  /**
   * 获取第一个校验错误信息.
   *
   * @param result the result
   * @return the string
   */
  public static String collectFirst(ViolationResult result) {
    List<ViolationErrors> errors = result.getViolationErrors();
    return errors.size() < 1 ? "" : errors.get(0).toString();
  }

  /**
   * 获取所有校验错误信息.
   *
   * @param result the result
   * @return the string
   */
  public static String collect(ViolationResult result) {
    return result.stream().map(ViolationErrors::toString).collect(Collectors.joining("\n"));
  }

  /**
   * 获取第一个校验错误信息.
   *
   * @param bindingResult the binding result
   * @return the string
   */
  public static String collectFirst(BindingResult bindingResult) {
    ObjectError error = bindingResult.getAllErrors().get(0);
    return Objects.requireNonNull(error.getDefaultMessage()).contains("Failed to convert property")
        ? ((FieldError) error).getField() + " 格式不正确"
        : error.getDefaultMessage();
  }

  /**
   * 获取所有校验错误信息.
   *
   * @param bindingResult the binding result
   * @return the string
   */
  public static String collect(BindingResult bindingResult) {
    StringBuilder errorMsg = new StringBuilder();
    bindingResult
        .getAllErrors()
        .forEach(
            error ->
                errorMsg
                    .append("<br/>")
                    .append(
                        Objects.requireNonNull(error.getDefaultMessage())
                                .contains("Failed to convert property")
                            ? ((FieldError) error).getField() + " 格式不正确"
                            : error.getDefaultMessage()));
    return errorMsg.toString().replaceFirst("<br/>", "");
  }

  /** 校验结果. */
  public static class ViolationResult implements Iterable<ViolationErrors> {
    /** The constant EMPTY. */
    public static final ViolationResult EMPTY = new ViolationResult();

    private final List<ViolationErrors> violationErrors = new ArrayList<>();

    /**
     * Has errors boolean.
     *
     * @return the boolean
     */
    public boolean hasErrors() {
      return violationErrors.size() > 0;
    }

    /**
     * Gets violation errors.
     *
     * @return the violation errors
     */
    public List<ViolationErrors> getViolationErrors() {
      return violationErrors;
    }

    /**
     * Add error.
     *
     * @param error the error
     */
    public void addError(@NonNull ViolationErrors error) {
      violationErrors.add(error);
    }

    /**
     * Add error.
     *
     * @param errors the errors
     */
    public void addError(@NonNull List<ViolationErrors> errors) {
      violationErrors.addAll(errors);
    }

    @Override
    @Nonnull
    public Iterator<ViolationErrors> iterator() {
      return new Iterator<ViolationErrors>() {
        private final AtomicInteger index = new AtomicInteger();

        @Override
        public final boolean hasNext() {
          return index.get() < violationErrors.size();
        }

        @Override
        public ViolationErrors next() {
          return violationErrors.get(index.getAndIncrement());
        }
      };
    }

    /**
     * Stream stream.
     *
     * @return the stream
     */
    public Stream<ViolationErrors> stream() {
      return StreamSupport.stream(spliterator(), false);
    }

    @Override
    public String toString() {
      return "ViolationResult{" + "violationErrors=" + violationErrors + '}';
    }
  }

  /** The type Violation errors. */
  @Getter
  public static class ViolationErrors {
    private String property;
    private String message;

    private ViolationErrors() {}

    /**
     * Of violation errors.
     *
     * @param property the property
     * @param message the message
     * @return the violation errors
     */
    public static ViolationErrors of(String property, String message) {
      ViolationErrors error = new ViolationErrors();
      error.property = property;
      error.message = message;
      return error;
    }

    @Override
    public String toString() {
      return message;
    }
  }
}
