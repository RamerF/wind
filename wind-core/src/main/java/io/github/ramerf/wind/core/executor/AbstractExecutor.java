package io.github.ramerf.wind.core.executor;

import org.springframework.jdbc.core.support.JdbcDaoSupport;

/**
 * The abstract executor.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/19
 */
public abstract class AbstractExecutor extends JdbcDaoSupport implements JdbcTemplateExecutor {}
