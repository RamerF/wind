package io.github.ramerf.wind.core.jdbc;

import java.sql.Connection;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;

/**
 * @author ramer
 * @since 24/02/2022
 */
public class ConnectionHolder {
  @Getter @Setter private Connection connection;
  @Getter @Setter private int referenceCount;

  public ConnectionHolder(@Nonnull Connection connection) {
    this.connection = connection;
  }

  public Connection requestConnection() {
    this.referenceCount++;
    return connection;
  }

  public void releaseConnection() {
    this.referenceCount--;
    if (referenceCount < 0) {
      this.connection = null;
      this.referenceCount = 0;
    }
  }
}
