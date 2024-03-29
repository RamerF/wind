/*
 * Copyright 2012-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.ramerf.wind.core;

/**
 * Copy from {@link org.springframework.boot.SpringBootVersion}.<br>
 * Class that exposes the Spring Boot version. Fetches the "Implementation-Version" manifest
 * attribute from the jar file.
 *
 * <p>Note that some ClassLoaders do not expose the package metadata, hence this class might not be
 * able to determine the Spring Boot version in all environments. Consider using a reflection-based
 * check instead: For example, checking for the presence of a specific Spring Boot method that you
 * intend to call.
 *
 * @author Drummond Dawson
 * @since 1.3.0
 */
public final class WindVersion {

  private WindVersion() {}

  /**
   * Return the full version string of the present Wind codebase, or {@code null} if it cannot be
   * determined.
   *
   * @return the version of Wind or {@code null}
   * @see Package#getImplementationVersion()
   */
  public static String getVersion() {
    Package pkg = WindVersion.class.getPackage();
    return (pkg != null) ? pkg.getImplementationVersion() : null;
  }
}
