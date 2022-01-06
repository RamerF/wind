package io.github.ramerf.wind.core.annotation;

import com.alibaba.fastjson.JSON;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.*;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.*;
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
    final Class<AutoConfigConfiguration> clazz = AutoConfigConfiguration.class;
    URL url =
        ConfigurationPropertiesTest.class.getClassLoader().getResource("application-mysql.yml");
    if (url == null) {
      return;
    }
    final FileInputStream inputStream = new FileInputStream(url.getFile());
    final Map<String, Object> configMap = yamlHandler(new FileInputStream[] {inputStream});

    final ConfigurationProperties annotation = clazz.getAnnotation(ConfigurationProperties.class);
    if (annotation == null) {
      return;
    }
    final String prefix = getPrefix(annotation);
    final Object obj = BeanUtils.initial(clazz);
    final List<Field> fields = BeanUtils.retrievePrivateFields(clazz, ArrayList::new);
    for (final Field field : fields) {
      final String key = prefix.concat(".").concat(field.getName());
      final Object value = configMap.get(key);
      retrievePopulateFields(obj, field, value, key, configMap);
    }
    log.info("test:[{}]", JSON.toJSONString(obj, true));
  }

  private void retrievePopulateFields(
      final Object obj,
      final Field field,
      final Object value,
      final String prefix,
      final Map<String, Object> configMap) {
    // 引用属性
    final Class<?> fieldType = field.getType();
    // 枚举使用name
    if (fieldType.isEnum()) {
      if (value == null) {
        return;
      }
      final Object[] enumConstants = fieldType.getEnumConstants();
      for (final Object enumConstant : enumConstants) {
        if (((Enum<?>) enumConstant).name().equalsIgnoreCase(value.toString())) {
          BeanUtils.setValue(obj, field, enumConstant, null);
        }
      }
      return;
    }
    if (fieldType.getClassLoader() != null
        && field.getAnnotation(NestedConfigurationProperties.class) != null) {
      final Object nestObj = BeanUtils.initial(fieldType);
      BeanUtils.setValue(obj, field, nestObj, null);
      final List<Field> nestFields = BeanUtils.retrievePrivateFields(fieldType, ArrayList::new);
      for (final Field nestField : nestFields) {
        final String key = prefix.concat(".").concat(nestField.getName());
        final Object nestValue = configMap.get(key);
        retrievePopulateFields(nestObj, nestField, nestValue, key, configMap);
      }
    } else if (value != null) {
      BeanUtils.setValue(obj, field, value, null);
    }
  }

  @Nonnull
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
      if (object instanceof Map) {
        @SuppressWarnings("unchecked")
        Map<String, Object> map = (Map<String, Object>) object;
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
          key = StringUtils.dashToCamel(key);
          // 数据类型匹配
          if (value instanceof String) {
            result.put(key, value);
          } else if (value instanceof Map) {
            // 如果是map,就继续读取
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) value;
            buildFlattenedMap(result, map, key);
          } else if (value instanceof Collection) {
            @SuppressWarnings("unchecked")
            Collection<Object> collection = (Collection<Object>) value;
            if (collection.isEmpty()) {
              result.put(key, "");
            } else {
              int count = 0;
              for (final Object object : collection) {
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
