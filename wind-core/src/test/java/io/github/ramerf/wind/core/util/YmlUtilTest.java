package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("读取配置 测试")
public class YmlUtilTest {
  @Test
  public void test() {
    final AutoConfigConfiguration autoConfigConfiguration =
        YmlUtil.getInstance()
            .ignoreInvalidValues(false)
            .ignoreUnknownFields(true)
            .process(AutoConfigConfiguration.class, "application-mysql2.yml");
    log.info("test:[{}]", autoConfigConfiguration);
  }
}
