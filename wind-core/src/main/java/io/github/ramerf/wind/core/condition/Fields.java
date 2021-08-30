package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Getter;

import static java.util.stream.Collectors.toList;

/**
 * 可用于指定一个操作包含/不包含的字段.
 *
 * @author ramer
 * @since 10/07/2021
 */
public class Fields<T> {
  @Getter private final Set<Field> includes = new HashSet<>();
  @Getter private final Set<Field> excludes = new HashSet<>();

  private Fields() {}

  public static <T> Fields<T> of() {
    return new Fields<>();
  }

  public static <T> Fields<T> of(Class<T> clazz) {
    return new Fields<>();
  }

  @SafeVarargs
  public final Fields<T> include(final IFunction<T, ?>... includeFields) {
    this.includes.addAll(
        Arrays.stream(includeFields).map(BeanFunction::getField).collect(toList()));
    return this;
  }

  public final Fields<T> include(final boolean include, final IFunction<T, ?> includeField) {
    if (include) {
      this.includes.add(includeField.getField());
    }
    return this;
  }

  @SafeVarargs
  public final Fields<T> exclude(@Nonnull final IFunction<T, ?>... excludeFields) {
    for (final IFunction<T, ?> function : excludeFields) {
      this.includes.remove(function.getField());
    }
    this.excludes.addAll(
        Arrays.stream(excludeFields).map(BeanFunction::getField).collect(toList()));
    return this;
  }

  public final Fields<T> exclude(
      final boolean exclude, @Nonnull final IFunction<T, ?> excludeField) {
    if (exclude) {
      this.includes.remove(excludeField.getField());
      this.excludes.add(excludeField.getField());
    }
    return this;
  }

  public Set<Field> getIncludeFields() {
    return includes.isEmpty() ? Collections.emptySet() : includes;
  }

  public Set<Field> getExcludeFields() {
    return excludes.isEmpty() ? Collections.emptySet() : excludes;
  }
}
