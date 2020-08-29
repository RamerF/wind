package io.github.ramerf.wind.core.pgsql;

import io.github.ramerf.wind.core.support.IdGenerator;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

/**
 * The type Test application.
 *
 * @author Tang xiaofeng
 */
@SpringBootApplication
public class PgsqlApplication {
  public static void main(String[] args) {
    SpringApplication.run(PgsqlApplication.class, args);
  }

  @Bean
  public IdGenerator autoIncrementGenerator() {
    // 自定义id生成策略,下方为数据库自增写法
    return o -> null;
  }
}
