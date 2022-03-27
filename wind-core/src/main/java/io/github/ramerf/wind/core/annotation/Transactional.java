package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一个service需要开启事务,该注解实现只是简单的手动提交事务逻辑.
 *
 * <pre>{@code
 * try {
 *  connection.setAutoCommit(false)
 *  // method body
 *  connection.commit(true);
 * } catch(Exception e) {
 *   if(e instanceof #rollbackFor) {
 *     connection.rollback();
 *   }
 * } finally {
 *   connection.close();
 * }
 * }</pre>
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface Transactional {
  /** 方法执行抛出指定异常时回滚. */
  Class<? extends Exception> rollbackFor() default Exception.class;
}
