package io.github.ramerf.wind.core.converter;

import java.util.*;

/**
 * java:List&lt;Long&gt; &lt;-&gt; jdbc:Long[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListLongArrayTypeConverter implements TypeConverter<List<Long>, Long[]> {
  @Override
  public Long[] convertToJdbc(List<Long> javaVal) {
    final Long[] empty = new Long[0];
    return Objects.nonNull(javaVal) ? javaVal.toArray(empty) : empty;
  }

  @Override
  public List<Long> covertFromJdbc(final Long[] jdbcVal, final Class<? extends List<Long>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }
}
