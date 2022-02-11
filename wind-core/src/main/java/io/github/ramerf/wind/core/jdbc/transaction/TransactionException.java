package io.github.ramerf.wind.core.jdbc.transaction;

import javax.persistence.PersistenceException;

public class TransactionException extends PersistenceException {
  private static final long serialVersionUID = -433589569461084605L;

  public TransactionException() {}

  public TransactionException(String message) {
    super(message);
  }

  public TransactionException(String message, Throwable cause) {
    super(message, cause);
  }

  public TransactionException(Throwable cause) {
    super(cause);
  }
}
