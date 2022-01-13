package io.github.ramerf.wind.core.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Resource utils.
 *
 * @since 2022.01.13
 * @author ramer
 */
@Slf4j
public class ResourceUtils {
  private static final String CLASSPATH_PREFIX = "classpath*:";
  public static final String JAR_URL_PREFIX = "jar:";
  public static final String FILE_URL_PREFIX = "file:";
  public static final String JAR_URL_SEPARATOR = "!/";

  public static Set<File> getFiles(String locationPattern) throws IOException {
    Asserts.notNull(locationPattern, "Location pattern must not be null");
    if (locationPattern.startsWith(CLASSPATH_PREFIX)) {
      // 包含通配符
      if (isPattern(locationPattern.substring(CLASSPATH_PREFIX.length()))) {
        return findPathMatchingFiles(locationPattern);
      } else {
        return findAllClassPathFiles(locationPattern.substring(CLASSPATH_PREFIX.length()));
      }
    }
    return Collections.emptySet();
  }

  private static Set<File> findPathMatchingFiles(String locationPattern) throws IOException {
    String rootDirPath = getRootDir(locationPattern);
    String subPattern = locationPattern.substring(rootDirPath.length());
    Set<File> rootDirResources = getFiles(rootDirPath);
    Set<File> result = new LinkedHashSet<>(16);
    for (File file : rootDirResources) {
      // rootDirResource = resolveRootDirResource(rootDirResource);
      // URL rootDirUrl = rootDirResource.getURL();
      // if (equinoxResolveMethod != null && rootDirUrl.getProtocol().startsWith("bundle")) {
      //   URL resolvedUrl =
      //       (URL) ReflectionUtils.invokeMethod(equinoxResolveMethod, null, rootDirUrl);
      //   if (resolvedUrl != null) {
      //     rootDirUrl = resolvedUrl;
      //   }
      //   rootDirResource = new UrlResource(rootDirUrl);
      // }
      // result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern));
    }
    if (log.isDebugEnabled()) {
      log.debug("Resolved location pattern [" + locationPattern + "] to resources " + result);
    }
    return result;
  }

  private static Set<File> findAllClassPathFiles(String location) throws IOException {
    String path = location;
    if (path.startsWith("/")) {
      path = path.substring(1);
    }
    Set<File> result = new LinkedHashSet<>(16);
    doFindAllClassPathFiles(path, result);
    if (log.isDebugEnabled()) {
      log.debug("Resolved classpath location [" + location + "] to resources " + result);
    }
    return result;
  }

  private static Set<File> doFindAllClassPathFiles(String path, final Set<File> result)
      throws IOException {
    ClassLoader cl = getClassLoader();
    Enumeration<URL> resourceUrls =
        (cl != null ? cl.getResources(path) : ClassLoader.getSystemResources(path));
    while (resourceUrls.hasMoreElements()) {
      URL url = resourceUrls.nextElement();
      log.info("doFindAllClassPathFiles:[{}]", url.getPath());
      if (!"file".equals(url.getProtocol())) {
        continue;
      }
      final File file = new File(url.getFile());
      if (file.exists()) {
        result.add(file);
      }
    }
    return result;
  }

  /** 获取资源的根路径 */
  public static String getRootDir(String location) {
    int prefixEnd = location.indexOf(':') + 1;
    int rootDirEnd = location.length();
    while (rootDirEnd > prefixEnd && isPattern(location.substring(prefixEnd, rootDirEnd))) {
      rootDirEnd = location.lastIndexOf('/', rootDirEnd - 2) + 1;
    }
    if (rootDirEnd == 0) {
      rootDirEnd = prefixEnd;
    }
    return location.substring(0, rootDirEnd);
  }

  /** 包含?或* */
  public static boolean isPattern(final String string) {
    return string.contains("*") || string.contains("?");
  }

  public static ClassLoader getClassLoader() {
    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    return classLoader == null ? ResourceUtils.class.getClassLoader() : classLoader;
  }
}
