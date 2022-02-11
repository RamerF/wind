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
  private YmlUtil() {}

  /** @see #process(Class, String, boolean) */
  public static <T> T process(final Class<T> clazz, final String resourcePath)
      throws CommonException {
    return process(clazz, resourcePath, true);
  }

  /**
   * 读取yml文件.
   *
   * @param clazz 需要填充的对象,必须包含默认构造器
   * @param resourcePath 类路径的相对路径,如 application.yml
   * @param ignoreInvalidValues 忽略配置中无效/错误的值
   * @return 如果{@code clazz}未包含注解{@link ConfigurationProperties},返回默认实例
   * @throws CommonException 文件读取失败时抛出.
   */
  public static <T> T process(
      final Class<T> clazz, final String resourcePath, final boolean ignoreInvalidValues)
      throws CommonException {
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
      final List<Field> fields = BeanUtils.retrievePrivateFields(clazz);
      for (final Field field : fields) {
        final String key = prefix.concat(".").concat(field.getName());
        final Object value = configMap.get(key);
        retrieveSetFields(obj, field, value, key, configMap, ignoreInvalidValues);
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
      final Map<String, Object> configMap,
      final boolean ignoreInvalidValues) {
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
          setValue(obj, field, enumConstant, ignoreInvalidValues);
          return;
        }
      }
      if (ignoreInvalidValues) {
        log.warn("Invalid enum value:{} for {}", value, fieldType);
        return;
      }
      throw new IllegalArgumentException("Invalid enum value " + value + " for " + fieldType);
    }
    if (fieldType.getClassLoader() != null
        && field.getAnnotation(NestedConfigurationProperties.class) != null) {
      final Object nestObj = BeanUtils.initial(fieldType);
      setValue(obj, field, nestObj, ignoreInvalidValues);
      @SuppressWarnings("DuplicatedCode")
      final List<Field> nestFields = BeanUtils.retrievePrivateFields(fieldType);
      for (final Field nestField : nestFields) {
        final String key = prefix.concat(".").concat(nestField.getName());
        final Object nestValue = configMap.get(key);
        retrieveSetFields(nestObj, nestField, nestValue, key, configMap, ignoreInvalidValues);
      }
    } else if (value != null) {
      setValue(obj, field, value, ignoreInvalidValues);
    }
  }

  private static void setValue(
      final Object obj, final Field field, final Object value, final boolean ignoreInvalidValues) {
    Object val;
    Class<?> clazz = field.getType();
    try {
      if (value == null) {
        val = null;
      } else if (value.getClass().equals(clazz)) {
        val = value;
      } else if (clazz.equals(String.class)) {
        val = String.valueOf(value);
      } else if (clazz.equals(short.class) || clazz.equals(Short.class)) {
        val = Short.valueOf(value.toString());
      } else if (clazz.equals(int.class) || clazz.equals(Integer.class)) {
        val = Integer.valueOf(value.toString());
      } else if (clazz.equals(long.class) || clazz.equals(Long.class)) {
        val = Long.valueOf(value.toString());
      } else if (clazz.equals(float.class) || clazz.equals(Float.class)) {
        val = Float.valueOf(value.toString());
      } else if (clazz.equals(double.class) || clazz.equals(Double.class)) {
        val = Double.valueOf(value.toString());
      } else if (clazz.equals(boolean.class) || clazz.equals(Boolean.class)) {
        val = Boolean.valueOf(value.toString());
      } else val = value;
    } catch (Exception e) {
      log.warn("Could not set value for field {}", field);
      if (ignoreInvalidValues) {
        val = null;
      } else throw e;
    }
    if (ignoreInvalidValues) {
      BeanUtils.setFieldValueIgnoreException(obj, field, val);
    } else {
      BeanUtils.setFieldValue(obj, field, val);
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
