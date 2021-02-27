package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import java.lang.annotation.*;

/**
 * 指定数据库表映射.
 *
 * @author ramer
 * @since 2019/12/29
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface TypeHandler {
  @SuppressWarnings("rawtypes")
  Class<? extends ITypeHandler> value();
}
