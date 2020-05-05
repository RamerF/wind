package io.github.ramerf.wind.core.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import lombok.extern.slf4j.Slf4j;

/**
 * 文件工具类.
 *
 * @author Tang Xiaofeng
 * @since 2020/2/29
 */
@Slf4j
public class FileUtils {
  /**
   * 将字节数组转换为文件并保存.
   *
   * @param bys 字节数组
   * @param dest 文件未位置
   */
  public static void write(byte[] bys, final String dest) {}

  /**
   * 将字节数组转换为临时文件并在执行额外操作后自动删除.
   *
   * @param bys 字节数组
   * @param suffix 文件后缀,如: jpg,默认为tmp
   * @param consumer 文件名
   */
  @SuppressWarnings("ResultOfMethodCallIgnored")
  public static <T> T writeTmpOps(byte[] bys, final String suffix, Function<File, T> consumer) {
    try {
      File file =
          File.createTempFile("tmpFile", StringUtils.isEmpty(suffix) ? null : ".".concat(suffix));
      Files.write(file.toPath(), bys, StandardOpenOption.WRITE);
      final T apply = consumer.apply(file);
      file.delete();
      return apply;
    } catch (IOException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return null;
  }
}
