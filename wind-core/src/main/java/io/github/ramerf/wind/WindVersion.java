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

package io.github.ramerf.wind;

/** Copy from {@link org.springframework.boot.SpringBootVersion}.<br> */
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
