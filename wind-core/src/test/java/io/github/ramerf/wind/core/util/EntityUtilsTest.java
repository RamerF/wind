package io.github.ramerf.wind.core.util;

import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@Slf4j
@DisplayName("EntityUtils 测试")
public class EntityUtilsTest {
  @Test
  public void testScanClasses() throws IOException {
    EntityUtils.getAllColumnFields(Ts.class).forEach(o -> System.out.println(o.getName()));
  }
}
