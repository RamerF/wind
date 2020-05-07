package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.Condition.MatchPattern;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IFunction;
import java.util.*;
import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.github.ramerf.wind.core.util.BeanUtils.methodToColumn;

/**
 * 通用业务方法.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /11/13
 */
@SuppressWarnings("unused")
public interface BaseService<T extends AbstractEntityPoJo>
    extends QueryService<T>, UpdateService<T> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(BaseService.class);

  /** 条件构造. */
  @Data
  @Deprecated
  class ExtraProp {

    /**
     * 获取条件构造器.
     *
     * @return the extra prop builder
     */
    public static ExtraPropBuilder builder() {
      return new ExtraPropBuilder();
    }

    /**
     * 获取条件构造器,带companyId条件.
     *
     * @param companyId the company id
     * @return the extra prop builder
     */
    public static ExtraPropBuilder builder(final long companyId) {
      return builder()
          .add(ExtraProp.of(AbstractEntityPoJo::getCompanyId, MatchPattern.EQUAL, companyId));
    }

    /** The type Extra prop builder. */
    public static class ExtraPropBuilder {
      private final List<ExtraProp> list = new ArrayList<>();

      /**
       * 添加条件.
       *
       * @param extraProp the extra prop
       * @return the extra prop builder
       */
      public ExtraPropBuilder add(final ExtraProp extraProp) {
        list.add(extraProp);
        return this;
      }

      /**
       * 动态添加条件.
       *
       * @param condition 为true时包含条件
       * @param extraProp the extra prop
       * @return the extra prop builder
       */
      public ExtraPropBuilder add(final boolean condition, final ExtraProp extraProp) {
        if (condition) {
          list.add(extraProp);
        }
        return this;
      }

      /**
       * 构造条件.
       *
       * @return the list
       */
      public List<ExtraProp> build() {
        return list;
      }
    }

    private String name;
    private MatchPattern matchPattern;
    private List<Object> value;
    /** 是否分组 group by */
    private boolean isGroup = false;

    /**
     * 替代方法: {@link BaseService.ExtraProp()#ofListValue(IFunction, MatchPattern, List)} @param <R>
     * the type parameter
     *
     * @param name the name
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    @SuppressWarnings("unchecked")
    private static <R> ExtraProp ofListValue(
        String name, MatchPattern matchPattern, List<R> value) {
      return copyValue(name, false, matchPattern, (List<Object>) value);
    }

    /**
     * 添加List类型的参数.
     *
     * @param <T> the type parameter
     * @param <R> the type parameter
     * @param function the function
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    public static <T extends AbstractEntityPoJo, R> ExtraProp ofListValue(
        IFunction<T, R> function, MatchPattern matchPattern, List<R> value) {
      return ofListValue(methodToColumn(function), matchPattern, value);
    }

    /**
     * 推荐使用lambda表达式,替代方法: {@link BaseService.ExtraProp()#of(IFunction, MatchPattern, Object...)}
     *
     * @param name the name
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    @Deprecated
    public static ExtraProp of(String name, MatchPattern matchPattern, Object... value) {
      if (value.length > 0 && value[0] instanceof Collection) {
        throw CommonException.of("参数不能为集合");
      }
      return copyValue(name, false, matchPattern, value);
    }

    /**
     * Of extra prop.
     *
     * @param <T> the type parameter
     * @param <R> the type parameter
     * @param function the function
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    public static <T extends AbstractEntityPoJo, R> ExtraProp of(
        IFunction<T, R> function, MatchPattern matchPattern, Object... value) {
      return of(methodToColumn(function), matchPattern, value);
    }

    /**
     * Of extra prop.
     *
     * @param name the name
     * @param isGroup the is group
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    @Deprecated
    public static ExtraProp of(
        final String name,
        final boolean isGroup,
        final MatchPattern matchPattern,
        Object... value) {
      return copyValue(name, isGroup, matchPattern, value);
    }

    /**
     * Of extra prop.
     *
     * @param <T> the type parameter
     * @param <R> the type parameter
     * @param function the function
     * @param isGroup the is group
     * @param matchPattern the match pattern
     * @param value the value
     * @return the extra prop
     */
    public static <T extends AbstractEntityPoJo, R> ExtraProp of(
        IFunction<T, R> function,
        final boolean isGroup,
        final MatchPattern matchPattern,
        Object... value) {
      return of(methodToColumn(function), isGroup, matchPattern, value);
    }

    private static ExtraProp copyValue(
        String name, boolean isGroup, MatchPattern matchPattern, Object[] value) {
      return copyValue(name, isGroup, matchPattern, new ArrayList<>(Arrays.asList(value)));
    }

    private static ExtraProp copyValue(
        String name, boolean isGroup, MatchPattern matchPattern, List<Object> value) {
      ExtraProp extraProp = new ExtraProp();
      extraProp.setName(name);
      extraProp.setMatchPattern(matchPattern);
      extraProp.setValue(value);
      extraProp.setGroup(isGroup);
      if (Objects.isNull(matchPattern)) {
        throw CommonException.of("匹配模式不能为空");
      }
      final List<Object> propValue = extraProp.getValue();
      if (propValue.size() == 0
          && (matchPattern.equals(MatchPattern.EQUAL)
              || matchPattern.equals(MatchPattern.NOT_EQUAL)
              || matchPattern.equals(MatchPattern.GREATER)
              || matchPattern.equals(MatchPattern.GE)
              || matchPattern.equals(MatchPattern.LESS)
              || matchPattern.equals(MatchPattern.LE)
              || matchPattern.equals(MatchPattern.LIKE)
              || matchPattern.equals(MatchPattern.LIKE_LEFT)
              || matchPattern.equals(MatchPattern.LIKE_RIGHT)
              || matchPattern.equals(MatchPattern.NOT_LIKE)
              || matchPattern.equals(MatchPattern.IN)
              || matchPattern.equals(MatchPattern.NOT_IN))) {
        throw CommonException.of(String.format("匹配模式为[%s]时,值不能为空", matchPattern.name()));
      }
      final boolean check =
          (matchPattern.equals(MatchPattern.BETWEEN)
                  || matchPattern.equals(MatchPattern.NOT_BETWEEN))
              && propValue.size() < 2;
      if (check) {
        throw CommonException.of(String.format("匹配模式为[%s]时,需要两个值", matchPattern.name()));
      }
      return extraProp;
    }
  }
}
