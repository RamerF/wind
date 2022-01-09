/*
 * Copyright 2002-2017 the original author or authors.
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

package io.github.ramerf.wind.core.util;

import javax.annotation.Nullable;

public final class Asserts {
  public static void notNull(final Object object, String message) {
    if (object == null) {
      throw new IllegalArgumentException(message);
    }
  }

  /**
   * Assert that the given String is not empty; that is, it must not be {@code null} and not the
   * empty String.
   *
   * <pre class="code">Assert.hasLength(name, "Name must not be empty");</pre>
   *
   * @param text the String to check
   * @param message the exception message to use if the assertion fails
   * @see StringUtils#hasLength
   * @throws IllegalArgumentException if the text is empty
   */
  public static void hasLength(@Nullable String text, String message) {
    if (!StringUtils.hasLength(text)) {
      throw new IllegalArgumentException(message);
    }
  }
}
