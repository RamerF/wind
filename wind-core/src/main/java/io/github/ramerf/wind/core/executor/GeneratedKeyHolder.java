/*
 * Copyright 2002-2018 the original author or authors.
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

package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.exception.TooManyResultException;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * 复制于: {@link org.springframework.jdbc.support.GeneratedKeyHolder}<br>
 * The standard implementation of the {@link KeyHolder} interface, to be used for holding
 * auto-generated keys (as potentially returned by JDBC insert statements).
 *
 * @author Thomas Risberg
 * @author Juergen Hoeller
 * @since 1.1
 */
public class GeneratedKeyHolder implements KeyHolder {

  private final List<Map<String, Object>> keyList;

  public GeneratedKeyHolder() {
    this.keyList = new LinkedList<>();
  }

  public GeneratedKeyHolder(List<Map<String, Object>> keyList) {
    this.keyList = keyList;
  }

  @Nonnull
  @Override
  public Map<String, Object> getKeys() throws TooManyResultException {
    if (this.keyList.isEmpty()) {
      return Collections.emptyMap();
    }
    if (this.keyList.size() > 1) {
      throw new TooManyResultException(
          "The getKeys method should only be used when keys for a single row are returned.  "
              + "The current key list contains keys for multiple rows: "
              + this.keyList,
          keyList.size());
    }
    return this.keyList.get(0);
  }

  @Override
  public List<Map<String, Object>> getKeyList() {
    return this.keyList;
  }
}
