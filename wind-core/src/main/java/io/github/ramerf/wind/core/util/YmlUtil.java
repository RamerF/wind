package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.ConfigurationProperties;
import io.github.ramerf.wind.core.annotation.NestedConfigurationProperties;
import io.github.ramerf.wind.core.exception.CommonException;
import java.io.*;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.reader.UnicodeReader;

@Slf4j
public final class YmlUtil {
  /** 忽略配置中的未知属性 */
  private boolean ignoreUnknownFields;
  /** 忽略配置中无效/错误的值 */
  private boolean ignoreInvalidValues;

  private YmlUtil() {}

  public static YmlUtil getInstance() {
    return new YmlUtil();
  }

  public YmlUtil ignoreUnknownFields(final boolean ignoreUnknownFields) {
    this.ignoreUnknownFields = ignoreUnknownFields;
    return this;
  }

  public YmlUtil ignoreInvalidValues(final boolean ignoreInvalidValues) {
    this.ignoreInvalidValues = ignoreInvalidValues;
    return this;
  }

  /**
   * 读取yml文件.
   *
   * @param clazz 需要填充的对象,必须包含默认构造器
   * @param resourcePath 类路径的相对路径,如 application.yml
   * @return 如果{@code clazz}未包含注解{@link ConfigurationProperties},返回默认实例
   * @throws CommonException 文件读取失败时抛出.
   */
  public <T> T process(final Class<T> clazz, final String resourcePath) throws CommonException {
    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    URL url =
        (classLoader == null ? YmlUtil.class.getClassLoader() : classLoader)
            .getResource(resourcePath);
    if (url == null) {
      throw new CommonException("Could not get resource from " + resourcePath);
    }
    try (final FileInputStream inputStream = new FileInputStream(url.getFile())) {
      final Map<String, Object> configMap = yamlHandler(new FileInputStream[] {inputStream});
      final ConfigurationProperties annotation = clazz.getAnnotation(ConfigurationProperties.class);
      if (annotation == null) {
        return null;
      }
      final String prefix = getPrefix(annotation);
      final T obj = BeanUtils.initial(clazz);
      final List<Field> fields = BeanUtils.retrievePrivateFields(clazz, ArrayList::new);
      for (final Field field : fields) {
        final String key = prefix.concat(".").concat(field.getName());
        final Object value = configMap.get(key);
        retrieveSetFields(obj, field, value, key, configMap);
      }
      return obj;
    } catch (IOException e) {
      throw new CommonException(e);
    }
  }

  /** 递归赋值 */
  private static void retrieveSetFields(
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
      @SuppressWarnings("DuplicatedCode")
      final List<Field> nestFields = BeanUtils.retrievePrivateFields(fieldType, ArrayList::new);
      for (final Field nestField : nestFields) {
        final String key = prefix.concat(".").concat(nestField.getName());
        final Object nestValue = configMap.get(key);
        retrieveSetFields(nestObj, nestField, nestValue, key, configMap);
      }
    } else if (value != null) {
      BeanUtils.setValue(obj, field, value, null);
    }
  }

  @Nonnull
  private static String getPrefix(final ConfigurationProperties annotation) {
    return annotation.prefix().equals("") ? annotation.value() : annotation.prefix();
  }

  /** yml文件处理 */
  private static Map<String, Object> yamlHandler(@Nonnull InputStream[] resources)
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

  /** 将yml文件配置属性使用.拼接 */
  private static void buildFlattenedMap(
      Map<String, Object> result, Map<String, Object> source, @Nullable String path) {
    for (Entry<String, Object> entry : source.entrySet()) {
      String key = entry.getKey();
      Object value = entry.getValue();
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
            buildFlattenedMap(result, Collections.singletonMap("[" + count++ + "]", object), key);
          }
        }
      } else {
        result.put(key, value != null ? value : "");
      }
    }
  }
}
