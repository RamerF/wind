package io.github.ramerf.wind.core.ioc;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;

/**
 * 资源加载器.
 *
 * @author ramer
 * @since 23/12/2021
 */
@Slf4j
public class ResourceResolver {

  public static Set<Class<?>> retrieveClasses(final String scanPackage) {
    Set<File> files;
    try {
      files = retrieveMatchingFiles(scanPackage);
    } catch (IOException e) {
      e.printStackTrace();
      log.info("Cannot search for class underneath directory [{}]", scanPackage);
      return Collections.emptySet();
    }
    // TODO WARN 扫描类
    //    return files.stream()
    //        .map(File::getPath)
    //        .map(o -> o.replace("/", "."))
    //        .collect(Collectors.toCollection(LinkedHashSet::new));
    return Collections.emptySet();
  }

  private static Set<URL> getUrls(final String path) throws IOException {
    Set<URL> urls = new HashSet<>();
    Enumeration<URL> resourceUrls = ResourceResolver.class.getClassLoader().getResources(path);
    while (resourceUrls.hasMoreElements()) {
      urls.add(resourceUrls.nextElement());
    }
    return urls;
  }

  private static Set<File> retrieveMatchingFiles(final String scanPackage) throws IOException {
    Set<URL> urls = getUrls(scanPackage);
    Set<File> result = new LinkedHashSet<>(50);
    for (URL url : urls) {
      if (url == null) {
        log.warn("no file found in[{}]", scanPackage);
        return Collections.emptySet();
      }
      // TODO WARN 判断是何种类型的资源,区分加载,否则可能打成jar无法找到class
      // org/springframework/core/io/support/PathMatchingResourcePatternResolver.java:508
      File rootDir = new File(url.getFile());
      if (!rootDir.exists()) {
        if (log.isDebugEnabled()) {
          log.debug("Skipping [" + rootDir.getAbsolutePath() + "] because it does not exist");
        }
        return Collections.emptySet();
      }
      if (!rootDir.isDirectory()) {
        if (log.isWarnEnabled()) {
          log.warn(
              "Skipping ["
                  + rootDir.getAbsolutePath()
                  + "] because it does not denote a directory");
        }
        return Collections.emptySet();
      }
      if (!rootDir.canRead()) {
        if (log.isWarnEnabled()) {
          log.warn(
              "Cannot search for matching files underneath directory ["
                  + rootDir.getAbsolutePath()
                  + "] because the application is not allowed to read the directory");
        }
        return Collections.emptySet();
      }
      doRetrieveMatchingFiles(rootDir, result);
    }
    return result;
  }

  private static void doRetrieveMatchingFiles(File dir, Set<File> result) {
    if (log.isDebugEnabled()) {
      log.debug("Searching directory [" + dir.getAbsolutePath() + "]");
    }
    File[] dirContents = dir.listFiles();
    if (dirContents == null) {
      if (log.isWarnEnabled()) {
        log.warn("Could not retrieve contents of directory [" + dir.getAbsolutePath() + "]");
      }
      return;
    }
    for (File content : dirContents) {
      if (content.isDirectory()) {
        if (!content.canRead()) {
          if (log.isDebugEnabled()) {
            log.debug(
                "Skipping subdirectory ["
                    + dir.getAbsolutePath()
                    + "] because the application is not allowed to read the directory");
          }
        } else {

          doRetrieveMatchingFiles(content, result);
        }
      } else result.add(content);
    }
  }

  public static void main(String[] args) {
    Set<Class<?>> strings = ResourceResolver.retrieveClasses("io/github/ramerf/wind");
    log.info(
        "main:[{}]",
        strings.stream().map(Class::getName).collect(Collectors.joining("\n", "\n", "\n")));
  }
}
