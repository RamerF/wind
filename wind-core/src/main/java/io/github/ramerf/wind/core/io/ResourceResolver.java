package io.github.ramerf.wind.core.io;

import io.github.ramerf.wind.core.util.*;
import java.io.*;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.net.*;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipException;
import javax.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Resource utils.
 *
 * @since 2022.01.13
 * @author ramer
 */
@Slf4j
public class ResourceResolver {
  public static final String CLASSPATH_URL_PREFIX = "classpath:";
  public static final String CLASSPATH_ALL_URL_PREFIX = "classpath*:";

  /** URL protocol for a file in the file system: "file" */
  public static final String URL_PROTOCOL_FILE = "file";
  /** URL prefix for loading from the file system: "file:" */
  public static final String FILE_URL_PREFIX = "file:";

  /** URL protocol for an entry from a jar file: "jar" */
  public static final String URL_PROTOCOL_JAR = "jar";

  public static final String JAR_URL_PREFIX = "jar:";

  /** URL protocol for an entry from a war file: "war" */
  public static final String URL_PROTOCOL_WAR = "war";

  /** URL prefix for loading from a war file on Tomcat: "war:" */
  public static final String WAR_URL_PREFIX = "war:";

  /** URL protocol for an entry from a zip file: "zip" */
  public static final String URL_PROTOCOL_ZIP = "zip";

  /** URL protocol for an entry from a WebSphere jar file: "wsjar" */
  public static final String URL_PROTOCOL_WSJAR = "wsjar";

  /** URL protocol for an entry from a JBoss jar file: "vfszip" */
  public static final String URL_PROTOCOL_VFSZIP = "vfszip";

  public static final String URL_PROTOCOL_VFS = "vfs";

  /** Special separator between WAR URL and jar part on Tomcat */
  public static final String WAR_URL_SEPARATOR = "*/";

  /** Separator between JAR URL and file path within the JAR: "!/" */
  public static final String JAR_URL_SEPARATOR = "!/";

  private static final char PATH_SEPARATOR = '/';
  private static final char PACKAGE_SEPARATOR = '.';
  private static final ResourceLoader resourceLoader = new DefaultResourceLoader();
  private static final PathMatcher PATH_MATCHER = new AntPathMatcher();
  @Nullable private static Method equinoxResolveMethod;

  static {
    try {
      // Detect Equinox OSGi (e.g. on WebSphere 6.1)
      Class<?> fileLocatorClass =
          ClassUtils.forName(
              "org.eclipse.core.runtime.FileLocator", ResourceResolver.class.getClassLoader());
      equinoxResolveMethod = fileLocatorClass.getMethod("resolve", URL.class);
      log.debug("Found Equinox FileLocator for OSGi bundle URL resolution");
    } catch (Throwable ex) {
      equinoxResolveMethod = null;
    }
  }

  public static ResourceLoader getResourceLoader() {
    return resourceLoader;
  }

  @Nullable
  public static ClassLoader getClassLoader() {
    return getResourceLoader().getClassLoader();
  }

  public static PathMatcher getPathMatcher() {
    return PATH_MATCHER;
  }

  public static Resource[] getResources(String locationPattern) throws IOException {
    Asserts.notNull(locationPattern, "Location pattern must not be null");
    if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) {
      // 包含通配符
      if (PATH_MATCHER.isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) {
        return findPathMatchingResources(locationPattern);
      } //
      else {
        return findAllClassPathResources(
            locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()));
      }
    } //
    else {
      // Generally only look for a pattern after a prefix here,
      // and on Tomcat only after the "*/" separator for its "war:" protocol.
      int prefixEnd =
          (locationPattern.startsWith("war:")
              ? locationPattern.indexOf("*/") + 1
              : locationPattern.indexOf(':') + 1);
      if (PATH_MATCHER.isPattern(locationPattern.substring(prefixEnd))) {
        // a file pattern
        return findPathMatchingResources(locationPattern);
      } else {
        // a single resource with the given name
        return new Resource[] {getResourceLoader().getResource(locationPattern)};
      }
    }
  }

  protected static Resource[] findAllClassPathResources(String location) throws IOException {
    String path = location;
    if (path.startsWith("/")) {
      path = path.substring(1);
    }
    Set<Resource> result = doFindAllClassPathResources(path);
    if (log.isDebugEnabled()) {
      log.debug("Resolved classpath location [" + location + "] to resources " + result);
    }
    return result.toArray(new Resource[0]);
  }

  private static Resource[] findPathMatchingResources(String locationPattern) throws IOException {
    String rootDirPath = determineRootDir(locationPattern);
    String subPattern = locationPattern.substring(rootDirPath.length());
    Resource[] rootDirResources = getResources(rootDirPath);
    Set<Resource> result = new LinkedHashSet<>(16);
    for (Resource rootDirResource : rootDirResources) {
      URL rootDirUrl = rootDirResource.getURL();
      if (equinoxResolveMethod != null && rootDirUrl.getProtocol().startsWith("bundle")) {
        URL resolvedUrl = (URL) BeanUtils.invokeMethod(null, equinoxResolveMethod, rootDirUrl);
        if (resolvedUrl != null) {
          rootDirUrl = resolvedUrl;
        }
        rootDirResource = new UrlResource(rootDirUrl);
      }
      if (rootDirUrl.getProtocol().startsWith(ResourceResolver.URL_PROTOCOL_VFS)) {
        result.addAll(
            VfsResourceMatchingDelegate.findMatchingResources(
                rootDirUrl, subPattern, PATH_MATCHER));
      } else if (ResourceResolver.isJarURL(rootDirUrl)) {
        result.addAll(doFindPathMatchingJarResources(rootDirResource, rootDirUrl, subPattern));
      } else {
        result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern));
      }
    }
    if (log.isDebugEnabled()) {
      log.debug("Resolved location pattern [" + locationPattern + "] to resources " + result);
    }
    return result.toArray(new Resource[0]);
  }

  protected static Set<Resource> doFindPathMatchingFileResources(
      Resource rootDirResource, String subPattern) throws IOException {

    File rootDir;
    try {
      rootDir = rootDirResource.getFile().getAbsoluteFile();
    } catch (FileNotFoundException ex) {
      if (log.isInfoEnabled()) {
        log.info(
            "Cannot search for matching files underneath "
                + rootDirResource
                + " in the file system: "
                + ex.getMessage());
      }
      return Collections.emptySet();
    } catch (Exception ex) {
      if (log.isWarnEnabled()) {
        log.warn("Failed to resolve " + rootDirResource + " in the file system: " + ex);
      }
      return Collections.emptySet();
    }
    return doFindMatchingFileSystemResources(rootDir, subPattern);
  }

  protected static Set<Resource> doFindMatchingFileSystemResources(File rootDir, String subPattern)
      throws IOException {
    if (log.isDebugEnabled()) {
      log.debug("Looking for matching resources in directory tree [" + rootDir.getPath() + "]");
    }
    Set<File> matchingFiles = retrieveMatchingFiles(rootDir, subPattern);
    Set<Resource> result = new LinkedHashSet<>(matchingFiles.size());
    for (File file : matchingFiles) {
      result.add(new FileSystemResource(file));
    }
    return result;
  }

  /**
   * Retrieve files that match the given path pattern, checking the given directory and its
   * subdirectories.
   *
   * @param rootDir the directory to start from
   * @param pattern the pattern to match against, relative to the root directory
   * @return a mutable Set of matching Resource instances
   * @throws IOException if directory contents could not be retrieved
   */
  protected static Set<File> retrieveMatchingFiles(File rootDir, String pattern)
      throws IOException {
    if (!rootDir.exists()) {
      // Silently skip non-existing directories.
      if (log.isDebugEnabled()) {
        log.debug("Skipping [" + rootDir.getAbsolutePath() + "] because it does not exist");
      }
      return Collections.emptySet();
    }
    if (!rootDir.isDirectory()) {
      // Complain louder if it exists but is no directory.
      if (log.isWarnEnabled()) {
        log.warn(
            "Skipping [" + rootDir.getAbsolutePath() + "] because it does not denote a directory");
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
    String fullPattern = rootDir.getAbsolutePath().replace(File.separator, "/");
    if (!pattern.startsWith("/")) {
      fullPattern += "/";
    }
    fullPattern = fullPattern + pattern.replace(File.separator, "/");
    Set<File> result = new LinkedHashSet<>(8);
    doRetrieveMatchingFiles(fullPattern, rootDir, result);
    return result;
  }

  /**
   * Recursively retrieve files that match the given pattern, adding them to the given result list.
   *
   * @param fullPattern the pattern to match against, with prepended root directory path
   * @param dir the current directory
   * @param result the Set of matching File instances to add to
   * @throws IOException if directory contents could not be retrieved
   */
  protected static void doRetrieveMatchingFiles(String fullPattern, File dir, Set<File> result)
      throws IOException {
    if (log.isDebugEnabled()) {
      log.debug(
          "Searching directory ["
              + dir.getAbsolutePath()
              + "] for files matching pattern ["
              + fullPattern
              + "]");
    }
    File[] dirContents = dir.listFiles();
    if (dirContents == null) {
      if (log.isWarnEnabled()) {
        log.warn("Could not retrieve contents of directory [" + dir.getAbsolutePath() + "]");
      }
      return;
    }
    Arrays.sort(dirContents);
    for (File content : dirContents) {
      String currPath = content.getAbsolutePath().replace(File.separator, "/");
      if (content.isDirectory() && getPathMatcher().matchStart(fullPattern, currPath + "/")) {
        if (!content.canRead()) {
          if (log.isDebugEnabled()) {
            log.debug(
                "Skipping subdirectory ["
                    + dir.getAbsolutePath()
                    + "] because the application is not allowed to read the directory");
          }
        } else {
          doRetrieveMatchingFiles(fullPattern, content, result);
        }
      }
      if (getPathMatcher().match(fullPattern, currPath)) {
        result.add(content);
      }
    }
  }

  protected static Set<Resource> doFindAllClassPathResources(String path) throws IOException {
    Set<Resource> result = new LinkedHashSet<>(16);
    ClassLoader cl = getClassLoader();
    Enumeration<URL> resourceUrls =
        (cl != null ? cl.getResources(path) : ClassLoader.getSystemResources(path));
    while (resourceUrls.hasMoreElements()) {
      URL url = resourceUrls.nextElement();
      result.add(convertClassLoaderURL(url));
    }
    if ("".equals(path)) {
      // The above result is likely to be incomplete, i.e. only containing file system references.
      // We need to have pointers to each of the jar files on the classpath as well...
      addAllClassLoaderJarRoots(cl, result);
    }
    return result;
  }

  protected static Resource convertClassLoaderURL(URL url) {
    return new UrlResource(url);
  }

  protected static void addAllClassLoaderJarRoots(
      @Nullable ClassLoader classLoader, Set<Resource> result) {
    if (classLoader instanceof URLClassLoader) {
      try {
        for (URL url : ((URLClassLoader) classLoader).getURLs()) {
          try {
            UrlResource jarResource = new UrlResource(JAR_URL_PREFIX + url + JAR_URL_SEPARATOR);
            if (jarResource.exists()) {
              result.add(jarResource);
            }
          } catch (MalformedURLException ex) {
            if (log.isDebugEnabled()) {
              log.debug(
                  "Cannot search for matching files underneath ["
                      + url
                      + "] because it cannot be converted to a valid 'jar:' URL: "
                      + ex.getMessage());
            }
          }
        }
      } catch (Exception ex) {
        if (log.isDebugEnabled()) {
          log.debug(
              "Cannot introspect jar files since ClassLoader ["
                  + classLoader
                  + "] does not support 'getURLs()': "
                  + ex);
        }
      }
    }

    if (classLoader == ClassLoader.getSystemClassLoader()) {
      // "java.class.path" manifest evaluation...
      addClassPathManifestEntries(result);
    }

    if (classLoader != null) {
      try {
        // Hierarchy traversal...
        addAllClassLoaderJarRoots(classLoader.getParent(), result);
      } catch (Exception ex) {
        if (log.isDebugEnabled()) {
          log.debug(
              "Cannot introspect jar files in parent ClassLoader since ["
                  + classLoader
                  + "] does not support 'getParent()': "
                  + ex);
        }
      }
    }
  }

  protected static void addClassPathManifestEntries(Set<Resource> result) {
    try {
      String javaClassPathProperty = System.getProperty("java.class.path");
      for (String path :
          PathUtils.delimitedListToStringArray(
              javaClassPathProperty, System.getProperty("path.separator"))) {
        try {
          String filePath = new File(path).getAbsolutePath();
          int prefixIndex = filePath.indexOf(':');
          if (prefixIndex == 1) {
            // Possibly "c:" drive prefix on Windows, to be upper-cased for proper duplicate
            // detection
            filePath = PathUtils.capitalize(filePath);
          }
          UrlResource jarResource =
              new UrlResource(
                  ResourceResolver.JAR_URL_PREFIX
                      + ResourceResolver.FILE_URL_PREFIX
                      + filePath
                      + ResourceResolver.JAR_URL_SEPARATOR);
          // Potentially overlapping with URLClassLoader.getURLs() result above!
          if (!result.contains(jarResource)
              && !hasDuplicate(filePath, result)
              && jarResource.exists()) {
            result.add(jarResource);
          }
        } catch (MalformedURLException ex) {
          if (log.isDebugEnabled()) {
            log.debug(
                "Cannot search for matching files underneath ["
                    + path
                    + "] because it cannot be converted to a valid 'jar:' URL: "
                    + ex.getMessage());
          }
        }
      }
    } catch (Exception ex) {
      if (log.isDebugEnabled()) {
        log.debug("Failed to evaluate 'java.class.path' manifest entries: " + ex);
      }
    }
  }

  private static boolean hasDuplicate(String filePath, Set<Resource> result) {
    if (result.isEmpty()) {
      return false;
    }
    String duplicatePath = (filePath.startsWith("/") ? filePath.substring(1) : "/" + filePath);
    try {
      return result.contains(
          new UrlResource(
              ResourceResolver.JAR_URL_PREFIX
                  + ResourceResolver.FILE_URL_PREFIX
                  + duplicatePath
                  + ResourceResolver.JAR_URL_SEPARATOR));
    } catch (MalformedURLException ex) {
      // Ignore: just for testing against duplicate.
      return false;
    }
  }

  /**
   * Find all resources in jar files that match the given location pattern via the Ant-style
   * PathMatcher.
   *
   * @param rootDirURL the pre-resolved root directory URL
   * @param subPattern the sub pattern to match (below the root directory)
   * @return a mutable Set of matching Resource instances
   * @throws IOException in case of I/O errors
   * @since 4.3
   * @see java.net.JarURLConnection
   */
  private static Set<Resource> doFindPathMatchingJarResources(
      Resource rootDirResource, URL rootDirURL, String subPattern) throws IOException {

    URLConnection con = rootDirURL.openConnection();
    JarFile jarFile;
    String jarFileUrl;
    String rootEntryPath;
    boolean closeJarFile;

    if (con instanceof JarURLConnection) {
      // Should usually be the case for traditional JAR files.
      JarURLConnection jarCon = (JarURLConnection) con;
      con.setUseCaches(con.getClass().getSimpleName().startsWith("JNLP"));

      jarFile = jarCon.getJarFile();
      jarFileUrl = jarCon.getJarFileURL().toExternalForm();
      JarEntry jarEntry = jarCon.getJarEntry();
      rootEntryPath = (jarEntry != null ? jarEntry.getName() : "");
      closeJarFile = !jarCon.getUseCaches();
    } else {
      // No JarURLConnection -> need to resort to URL file parsing.
      // We'll assume URLs of the format "jar:path!/entry", with the protocol
      // being arbitrary as long as following the entry format.
      // We'll also handle paths with and without leading "file:" prefix.
      String urlFile = rootDirURL.getFile();
      try {
        int separatorIndex = urlFile.indexOf(ResourceResolver.WAR_URL_SEPARATOR);
        if (separatorIndex == -1) {
          separatorIndex = urlFile.indexOf(ResourceResolver.JAR_URL_SEPARATOR);
        }
        if (separatorIndex != -1) {
          jarFileUrl = urlFile.substring(0, separatorIndex);
          rootEntryPath = urlFile.substring(separatorIndex + 2); // both separators are 2 chars
          jarFile = getJarFile(jarFileUrl);
        } else {
          jarFile = new JarFile(urlFile);
          jarFileUrl = urlFile;
          rootEntryPath = "";
        }
        closeJarFile = true;
      } catch (ZipException ex) {
        if (log.isDebugEnabled()) {
          log.debug("Skipping invalid jar classpath entry [" + urlFile + "]");
        }
        return Collections.emptySet();
      }
    }

    try {
      if (log.isDebugEnabled()) {
        log.debug("Looking for matching resources in jar file [" + jarFileUrl + "]");
      }
      if (!"".equals(rootEntryPath) && !rootEntryPath.endsWith("/")) {
        // Root entry path must end with slash to allow for proper matching.
        // The Sun JRE does not return a slash here, but BEA JRockit does.
        rootEntryPath = rootEntryPath + "/";
      }
      Set<Resource> result = new LinkedHashSet<>(8);
      for (Enumeration<JarEntry> entries = jarFile.entries(); entries.hasMoreElements(); ) {
        JarEntry entry = entries.nextElement();
        String entryPath = entry.getName();
        if (entryPath.startsWith(rootEntryPath)) {
          String relativePath = entryPath.substring(rootEntryPath.length());
          if (PATH_MATCHER.match(subPattern, relativePath)) {
            result.add(rootDirResource.createRelative(relativePath));
          }
        }
      }
      return result;
    } finally {
      if (closeJarFile) {
        jarFile.close();
      }
    }
  }

  /** Resolve the given jar file URL into a JarFile object. */
  protected static JarFile getJarFile(String jarFileUrl) throws IOException {
    if (jarFileUrl.startsWith(ResourceResolver.FILE_URL_PREFIX)) {
      try {
        return new JarFile(new URI(jarFileUrl.replace(" ", "%20")).getSchemeSpecificPart());
      } catch (URISyntaxException ex) {
        // Fallback for URLs that are not valid URIs (should hardly ever happen).
        return new JarFile(jarFileUrl.substring(ResourceResolver.FILE_URL_PREFIX.length()));
      }
    } else {
      return new JarFile(jarFileUrl);
    }
  }

  /**
   * Resolve the given resource URI to a {@code java.io.File}, i.e. to a file in the file system.
   *
   * @param resourceUri the resource URI to resolve
   * @param description a description of the original resource that the URI was created for (for
   *     example, a class path location)
   * @return a corresponding File object
   * @throws FileNotFoundException if the URL cannot be resolved to a file in the file system
   * @since 2.5
   */
  public static File getFile(URI resourceUri, String description) throws FileNotFoundException {
    Asserts.notNull(resourceUri, "Resource URI must not be null");
    if (!URL_PROTOCOL_FILE.equals(resourceUri.getScheme())) {
      throw new FileNotFoundException(
          description
              + " cannot be resolved to absolute file path "
              + "because it does not reside in the file system: "
              + resourceUri);
    }
    return new File(resourceUri.getSchemeSpecificPart());
  }

  public static File getFile(URL resourceUrl, String description) throws FileNotFoundException {
    Asserts.notNull(resourceUrl, "Resource URL must not be null");
    if (!URL_PROTOCOL_FILE.equals(resourceUrl.getProtocol())) {
      throw new FileNotFoundException(
          description
              + " cannot be resolved to absolute file path "
              + "because it does not reside in the file system: "
              + resourceUrl);
    }
    try {
      return new File(toURI(resourceUrl).getSchemeSpecificPart());
    } catch (URISyntaxException ex) {
      // Fallback for URLs that are not valid URIs (should hardly ever happen).
      return new File(resourceUrl.getFile());
    }
  }

  /**
   * Extract the URL for the outermost archive from the given jar/war URL (which may point to a
   * resource in a jar file or to a jar file itself).
   *
   * <p>In the case of a jar file nested within a war file, this will return a URL to the war file
   * since that is the one resolvable in the file system.
   *
   * @param jarUrl the original URL
   * @return the URL for the actual jar file
   * @throws MalformedURLException if no valid jar file URL could be extracted
   * @since 4.1.8
   * @see #extractJarFileURL(URL)
   */
  public static URL extractArchiveURL(URL jarUrl) throws MalformedURLException {
    String urlFile = jarUrl.getFile();

    int endIndex = urlFile.indexOf(WAR_URL_SEPARATOR);
    if (endIndex != -1) {
      // Tomcat's "war:file:...mywar.war*/WEB-INF/lib/myjar.jar!/myentry.txt"
      String warFile = urlFile.substring(0, endIndex);
      if (URL_PROTOCOL_WAR.equals(jarUrl.getProtocol())) {
        return new URL(warFile);
      }
      int startIndex = warFile.indexOf(WAR_URL_PREFIX);
      if (startIndex != -1) {
        return new URL(warFile.substring(startIndex + WAR_URL_PREFIX.length()));
      }
    }

    // Regular "jar:file:...myjar.jar!/myentry.txt"
    return extractJarFileURL(jarUrl);
  }

  /**
   * Extract the URL for the actual jar file from the given URL (which may point to a resource in a
   * jar file or to a jar file itself).
   *
   * @param jarUrl the original URL
   * @return the URL for the actual jar file
   * @throws MalformedURLException if no valid jar file URL could be extracted
   */
  public static URL extractJarFileURL(URL jarUrl) throws MalformedURLException {
    String urlFile = jarUrl.getFile();
    int separatorIndex = urlFile.indexOf(JAR_URL_SEPARATOR);
    if (separatorIndex != -1) {
      String jarFile = urlFile.substring(0, separatorIndex);
      try {
        return new URL(jarFile);
      } catch (MalformedURLException ex) {
        // Probably no protocol in original jar URL, like "jar:C:/mypath/myjar.jar".
        // This usually indicates that the jar file resides in the file system.
        if (!jarFile.startsWith("/")) {
          jarFile = "/" + jarFile;
        }
        return new URL(FILE_URL_PREFIX + jarFile);
      }
    } else {
      return jarUrl;
    }
  }

  /**
   * Create a URI instance for the given URL, replacing spaces with "%20" URI encoding first.
   *
   * @param url the URL to convert into a URI instance
   * @return the URI instance
   * @throws URISyntaxException if the URL wasn't a valid URI
   * @see java.net.URL#toURI()
   */
  public static URI toURI(URL url) throws URISyntaxException {
    return toURI(url.toString());
  }

  /**
   * Create a URI instance for the given location String, replacing spaces with "%20" URI encoding
   * first.
   *
   * @param location the location String to convert into a URI instance
   * @return the URI instance
   * @throws URISyntaxException if the location wasn't a valid URI
   */
  public static URI toURI(String location) throws URISyntaxException {
    return new URI(location.replace(" ", "%20"));
  }

  /**
   * Set the {@link URLConnection#setUseCaches "useCaches"} flag on the given connection, preferring
   * {@code false} but leaving the flag at {@code true} for JNLP based resources.
   *
   * @param con the URLConnection to set the flag on
   */
  public static void useCachesIfNecessary(URLConnection con) {
    con.setUseCaches(con.getClass().getSimpleName().startsWith("JNLP"));
  }

  /** 获取资源的根路径 */
  protected static String determineRootDir(String location) {
    int prefixEnd = location.indexOf(':') + 1;
    int rootDirEnd = location.length();
    while (rootDirEnd > prefixEnd
        && getPathMatcher().isPattern(location.substring(prefixEnd, rootDirEnd))) {
      rootDirEnd = location.lastIndexOf('/', rootDirEnd - 2) + 1;
    }
    if (rootDirEnd == 0) {
      rootDirEnd = prefixEnd;
    }
    return location.substring(0, rootDirEnd);
  }

  /** 转换为资源路径 */
  public static String convertToResourcePath(final String path) {
    Asserts.notNull(path, "path must not be null");
    return path.replace(PACKAGE_SEPARATOR, PATH_SEPARATOR);
  }

  public static boolean isJarURL(URL url) {
    String protocol = url.getProtocol();
    return (URL_PROTOCOL_JAR.equals(protocol)
        || URL_PROTOCOL_WAR.equals(protocol)
        || URL_PROTOCOL_ZIP.equals(protocol)
        || URL_PROTOCOL_VFSZIP.equals(protocol)
        || URL_PROTOCOL_WSJAR.equals(protocol));
  }

  /**
   * Actually match the given {@code path} against the given {@code pattern}.
   *
   * @param pattern the pattern to match against
   * @param path the path String to test
   * @return {@code true} if the supplied {@code path} matched, {@code false} if it didn't
   */
  protected static boolean match(String pattern, String path) {
    final String pathSeparator = "/";
    if (path.startsWith(pathSeparator) != pattern.startsWith(pathSeparator)) {
      return false;
    }

    String[] pattDirs = StringUtils.tokenizeToStringArray(pattern, pathSeparator, false, true);

    String[] pathDirs = StringUtils.tokenizeToStringArray(path, pathSeparator, false, true);

    int pattIdxStart = 0;
    int pattIdxEnd = pattDirs.length - 1;
    int pathIdxStart = 0;
    int pathIdxEnd = pathDirs.length - 1;

    // Match all elements up to the first **
    while (pattIdxStart <= pattIdxEnd && pathIdxStart <= pathIdxEnd) {
      String pattDir = pattDirs[pattIdxStart];
      if ("**".equals(pattDir)) {
        break;
      }
      if (!matchStrings(pattDir, pathDirs[pathIdxStart])) {
        return false;
      }
      pattIdxStart++;
      pathIdxStart++;
    }

    if (pathIdxStart > pathIdxEnd) {
      // Path is exhausted, only match if rest of pattern is * or **'s
      if (pattIdxStart > pattIdxEnd) {
        return (pattern.endsWith(pathSeparator) == path.endsWith(pathSeparator));
      }
      if (pattIdxStart == pattIdxEnd
          && pattDirs[pattIdxStart].equals("*")
          && path.endsWith(pathSeparator)) {
        return true;
      }
      for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
        if (!pattDirs[i].equals("**")) {
          return false;
        }
      }
      return true;
    } else if (pattIdxStart > pattIdxEnd) {
      // String not exhausted, but pattern is. Failure.
      return false;
    }

    // up to last '**'
    while (pattIdxStart <= pattIdxEnd && pathIdxStart <= pathIdxEnd) {
      String pattDir = pattDirs[pattIdxEnd];
      if (pattDir.equals("**")) {
        break;
      }
      if (!matchStrings(pattDir, pathDirs[pathIdxEnd])) {
        return false;
      }
      pattIdxEnd--;
      pathIdxEnd--;
    }
    if (pathIdxStart > pathIdxEnd) {
      // String is exhausted
      for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
        if (!pattDirs[i].equals("**")) {
          return false;
        }
      }
      return true;
    }

    while (pattIdxStart != pattIdxEnd && pathIdxStart <= pathIdxEnd) {
      int patIdxTmp = -1;
      for (int i = pattIdxStart + 1; i <= pattIdxEnd; i++) {
        if (pattDirs[i].equals("**")) {
          patIdxTmp = i;
          break;
        }
      }
      if (patIdxTmp == pattIdxStart + 1) {
        // '**/**' situation, so skip one
        pattIdxStart++;
        continue;
      }
      // Find the pattern between padIdxStart & padIdxTmp in str between
      // strIdxStart & strIdxEnd
      int patLength = (patIdxTmp - pattIdxStart - 1);
      int strLength = (pathIdxEnd - pathIdxStart + 1);
      int foundIdx = -1;

      strLoop:
      for (int i = 0; i <= strLength - patLength; i++) {
        for (int j = 0; j < patLength; j++) {
          String subPat = pattDirs[pattIdxStart + j + 1];
          String subStr = pathDirs[pathIdxStart + i + j];
          if (!matchStrings(subPat, subStr)) {
            continue strLoop;
          }
        }
        foundIdx = pathIdxStart + i;
        break;
      }

      if (foundIdx == -1) {
        return false;
      }

      pattIdxStart = patIdxTmp;
      pathIdxStart = foundIdx + patLength;
    }

    for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
      if (!pattDirs[i].equals("**")) {
        return false;
      }
    }

    return true;
  }

  public static boolean matchStrings(String patternStr, String str) {
    StringBuilder patternBuilder = new StringBuilder();
    final Pattern GLOB_PATTERN =
        Pattern.compile("\\?|\\*|\\{((?:\\{[^/]+?\\}|[^/{}]|\\\\[{}])+?)\\}");
    final String DEFAULT_VARIABLE_PATTERN = "(.*)";
    Matcher matcher = GLOB_PATTERN.matcher(patternStr);
    int end = 0;
    while (matcher.find()) {
      patternBuilder.append(quote(patternStr, end, matcher.start()));
      String match = matcher.group();
      if ("?".equals(match)) {
        patternBuilder.append('.');
      } else if ("*".equals(match)) {
        patternBuilder.append(".*");
      } else if (match.startsWith("{") && match.endsWith("}")) {
        int colonIdx = match.indexOf(':');
        if (colonIdx == -1) {
          patternBuilder.append(DEFAULT_VARIABLE_PATTERN);
        } else {
          String variablePattern = match.substring(colonIdx + 1, match.length() - 1);
          patternBuilder.append('(');
          patternBuilder.append(variablePattern);
          patternBuilder.append(')');
          String variableName = match.substring(1, colonIdx);
        }
      }
      end = matcher.end();
    }
    patternBuilder.append(quote(patternStr, end, patternStr.length()));
    return Pattern.compile(patternBuilder.toString()).matcher(str).matches();
  }

  private static String quote(String s, int start, int end) {
    if (start == end) {
      return "";
    }
    return Pattern.quote(s.substring(start, end));
  }

  private static class VfsResourceMatchingDelegate {

    public static Set<Resource> findMatchingResources(
        URL rootDirURL, String locationPattern, PathMatcher pathMatcher) throws IOException {

      Object root = VfsPatternUtils.findRoot(rootDirURL);
      PatternVirtualFileVisitor visitor =
          new PatternVirtualFileVisitor(
              VfsPatternUtils.getPath(root), locationPattern, pathMatcher);
      VfsPatternUtils.visit(root, visitor);
      return visitor.getResources();
    }
  }

  /** VFS visitor for path matching purposes. */
  @SuppressWarnings("unused")
  private static class PatternVirtualFileVisitor implements InvocationHandler {

    private final String subPattern;

    private final PathMatcher pathMatcher;

    private final String rootPath;

    private final Set<Resource> resources = new LinkedHashSet<>();

    public PatternVirtualFileVisitor(String rootPath, String subPattern, PathMatcher pathMatcher) {
      this.subPattern = subPattern;
      this.pathMatcher = pathMatcher;
      this.rootPath = (rootPath.isEmpty() || rootPath.endsWith("/") ? rootPath : rootPath + "/");
    }

    @Override
    @Nullable
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      String methodName = method.getName();
      if (Object.class == method.getDeclaringClass()) {
        if (methodName.equals("equals")) {
          // Only consider equal when proxies are identical.
          return (proxy == args[0]);
        } else if (methodName.equals("hashCode")) {
          return System.identityHashCode(proxy);
        }
      } else if ("getAttributes".equals(methodName)) {
        return getAttributes();
      } else if ("visit".equals(methodName)) {
        visit(args[0]);
        return null;
      } else if ("toString".equals(methodName)) {
        return toString();
      }

      throw new IllegalStateException("Unexpected method invocation: " + method);
    }

    public void visit(Object vfsResource) {
      if (this.pathMatcher.match(
          this.subPattern,
          VfsPatternUtils.getPath(vfsResource).substring(this.rootPath.length()))) {
        this.resources.add(new VfsResource(vfsResource));
      }
    }

    @Nullable
    public Object getAttributes() {
      return VfsPatternUtils.getVisitorAttributes();
    }

    public Set<Resource> getResources() {
      return this.resources;
    }

    public int size() {
      return this.resources.size();
    }

    @Override
    public String toString() {
      return "sub-pattern: " + this.subPattern + ", resources: " + this.resources;
    }
  }
}
