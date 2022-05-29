package io.github.ramerf.wind.core.io;

import java.io.*;
import java.net.URI;
import java.net.URL;
import java.nio.channels.ReadableByteChannel;
import javax.annotation.Nullable;

/**
 * The interface Resource.
 *
 * @since 2022.05.29
 * @author ramer
 */
public interface Resource {
  boolean exists();

  InputStream getInputStream() throws IOException;

  String getDescription();

  boolean isReadable();

  boolean isOpen();

  boolean isFile();

  URL getURL() throws IOException;

  ReadableByteChannel readableChannel() throws IOException;

  long contentLength() throws IOException;

  long lastModified() throws IOException;

  Resource createRelative(String relativePath) throws IOException;

  URI getURI() throws IOException;

  File getFile() throws IOException;

  @Nullable
  String getFilename();
}
