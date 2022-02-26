package io.github.ramerf.wind.core.util;

import com.alibaba.fastjson.JSON;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@Slf4j
@DisplayName("读取配置 测试")
public class YmlUtilTest {
  @Test
  public void testProcess() {
    final AutoConfigConfiguration autoConfigConfiguration =
        YmlUtil.process(AutoConfigConfiguration.class, "wind.yml");
    log.info(JSON.toJSONString(autoConfigConfiguration, true));
  }
}
