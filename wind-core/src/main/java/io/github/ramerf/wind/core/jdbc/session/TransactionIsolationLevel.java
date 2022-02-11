package io.github.ramerf.wind.core.jdbc.session;

public enum TransactionIsolationLevel {
  NONE(0),
  READ_COMMITTED(2),
  READ_UNCOMMITTED(1),
  REPEATABLE_READ(4),
  SERIALIZABLE(8),
  SQL_SERVER_SNAPSHOT(4096);

  private final int level;

  TransactionIsolationLevel(int level) {
    this.level = level;
  }

  public int getLevel() {
    return this.level;
  }
}