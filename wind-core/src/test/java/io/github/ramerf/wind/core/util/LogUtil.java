package io.github.ramerf.wind.core.util;

import ch.qos.logback.classic.*;
import java.util.List;
import java.util.stream.Collectors;
import org.slf4j.LoggerFactory;

/**
 * @author ramer
 * @since 2022.03.19
 */
public class LogUtil {
  public static void setLoggerLevel(Class<?> clazz, Level level) {
    setLoggerLevel(clazz.getName(), level);
  }

  public static void setLoggerLevel(String loggerPackage, Level level) {
    LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
    List<Logger> loggers = loggerContext.getLoggerList();
    List<Logger> packageLoggerList =
        loggers.stream()
            .filter(a -> a.getName().startsWith(loggerPackage))
            .collect(Collectors.toList());
    if (packageLoggerList.isEmpty()) {
      loggerContext.getLogger(loggerPackage).setLevel(level);
      return;
    }
    for (Logger logger : packageLoggerList) {
      logger.setLevel(level);
    }
  }
}
