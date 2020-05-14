package io.github.ramerf.wind;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@MapperScan({"io.github.ramerf.wind.test.repository", "io.github.ramerf.wind.core.repository"})
@SpringBootApplication(scanBasePackages = {"io.github.ramerf.wind"})
public class TestApplication {
  public static void main(String[] args) {
    SpringApplication.run(TestApplication.class, args);
  }
}
