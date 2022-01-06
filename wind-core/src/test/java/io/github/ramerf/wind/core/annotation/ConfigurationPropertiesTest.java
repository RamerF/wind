package io.github.ramerf.wind.core.annotation;

import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.*;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.reader.UnicodeReader;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("读取配置 测试")
public class ConfigurationPropertiesTest {
  @Test
  public void test() throws IOException {
    URL url =
        ConfigurationPropertiesTest.class.getClassLoader().getResource("application-mysql.yml");
    if (url == null) {
      return;
    }
    final FileInputStream inputStream = new FileInputStream(url.getFile());
    final Map<String, Object> map = yamlHandler(new FileInputStream[] {inputStream});
    final ConfigurationProperties annotation =
        ConfigurationBean.class.getAnnotation(ConfigurationProperties.class);
    final String prefix = getPrefix(annotation);
    final List<Field> fields =
        BeanUtils.retrievePrivateFields(ConfigurationBean.class, ArrayList::new);
    final Set<Entry<String, Object>> entries = map.entrySet();
    for (Entry<String, Object> entry : entries) {
      final String key = entry.getKey();
      final Object value = entry.getValue();
      if (key.startsWith(prefix)) {}
    }
  }

  public static String getPrefix(final ConfigurationProperties annotation) {
    return annotation.prefix().equals("") ? annotation.value() : annotation.prefix();
  }

  /** 单个yaml文件处理 */
  public static Map<String, Object> yamlHandler(@Nonnull InputStream[] resources)
      throws IOException {
    Map<String, Object> result = new LinkedHashMap<>();
    Yaml yaml = new Yaml();
    Iterator<InputStream> iterator = Arrays.stream(resources).iterator();
    while (iterator.hasNext()) {
      InputStream resource = iterator.next();
      UnicodeReader reader = new UnicodeReader(resource);
      Object object = yaml.load(reader);
      // 这里只是简单处理，需要多个方式可以自己添加
      if (object instanceof Map) {
        Map map = (Map) object;
        buildFlattenedMap(result, map, null);
      }
      reader.close();
    }
    return result;
  }

  /** 这部分代码来至springboot源码部分对yaml的解析 YamlProcessor.java buildFlattenedMap方法 */
  private static void buildFlattenedMap(
      Map<String, Object> result, Map<String, Object> source, @Nullable String path) {
    // 循环读取原数据
    source.forEach(
        (key, value) -> {
          // 如果存在路径进行拼接
          if (StringUtils.hasText(path)) {
            if (key.startsWith("[")) {
              key = path + key;
            } else {
              key = path + '.' + key;
            }
          }
          // 数据类型匹配
          if (value instanceof String) {
            result.put(key, value);
          } else if (value instanceof Map) {
            // 如果是map,就继续读取
            Map<String, Object> map = (Map) value;
            buildFlattenedMap(result, map, key);
          } else if (value instanceof Collection) {
            Collection<Object> collection = (Collection) value;
            if (collection.isEmpty()) {
              result.put(key, "");
            } else {
              int count = 0;
              Iterator var7 = collection.iterator();
              while (var7.hasNext()) {
                Object object = var7.next();
                buildFlattenedMap(
                    result, Collections.singletonMap("[" + count++ + "]", object), key);
              }
            }
          } else {
            result.put(key, value != null ? value : "");
          }
        });
  }
}
