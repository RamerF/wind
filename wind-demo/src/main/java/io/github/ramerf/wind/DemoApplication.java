package io.github.ramerf.wind;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * 启动类.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@EnableTransactionManagement
@EntityScan("io.github.ramerf.wind.demo.entity.pojo")
@SpringBootApplication(
    scanBasePackages = {"io.github.ramerf.wind", "io.github.ramerf.wind.demo.repository"})
public class DemoApplication {
  public static void main(String[] args) {
    SpringApplication.run(DemoApplication.class, args);
  }
}
