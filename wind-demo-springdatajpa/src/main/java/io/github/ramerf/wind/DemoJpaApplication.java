package io.github.ramerf.wind;

import javax.persistence.Entity;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * 启动类.<br>
 * 注意: 与jpa/hibernate/springdatajpa整合时,一定要指定扫描entity包,同时配置文件最好指定wind.entity-package<br>
 * 因为poJo到表的映射也使用了{@link Entity}
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@EnableTransactionManagement
@EntityScan("io.github.ramerf.wind.demo.entity.domain")
@SpringBootApplication
public class DemoJpaApplication {
  public static void main(String[] args) {
    SpringApplication.run(DemoJpaApplication.class, args);
  }
}
