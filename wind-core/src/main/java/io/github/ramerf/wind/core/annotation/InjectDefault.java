package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * 注入默认实现.
 *
 * @author ramer
 * @since 2020/3/2
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({
  ElementType.FIELD,
  ElementType.METHOD,
  ElementType.PARAMETER,
  ElementType.TYPE,
  ElementType.ANNOTATION_TYPE
})
@Qualifier
public @interface InjectDefault {}
