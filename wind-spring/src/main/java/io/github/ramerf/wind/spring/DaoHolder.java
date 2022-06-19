package io.github.ramerf.wind.spring;

import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.util.Asserts;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.transaction.support.ResourceHolderSupport;

public final class DaoHolder extends ResourceHolderSupport {

  private final Dao dao;

  private final PersistenceExceptionTranslator exceptionTranslator;

  public DaoHolder(Dao dao, PersistenceExceptionTranslator exceptionTranslator) {
    Asserts.notNull(dao, "Dao must not be null");
    this.dao = dao;
    this.exceptionTranslator = exceptionTranslator;
  }

  public Dao getDao() {
    return dao;
  }

  public PersistenceExceptionTranslator getPersistenceExceptionTranslator() {
    return exceptionTranslator;
  }
}
