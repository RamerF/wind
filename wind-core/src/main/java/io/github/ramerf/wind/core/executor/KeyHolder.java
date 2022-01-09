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
import java.sql.Statement;
import java.util.List;
import java.util.Map;
import javax.annotation.Nonnull;

/**
 * 复制于:{@link org.springframework.jdbc.support.KeyHolder}<br>
 * Interface for retrieving keys, typically used for auto-generated keys as potentially returned by
 * JDBC insert statements.
 *
 * <p>Implementations of this interface can hold any number of keys. In the general case, the keys
 * are returned as a List containing one Map for each row of keys.
 *
 * <p>Most applications only use on key per row and process only one row at a time in an insert
 * statement. In these cases, just call {@code getKey} to retrieve the key. The returned value is a
 * Number here, which is the usual type for auto-generated keys.
 *
 * @author Thomas Risberg
 * @author Juergen Hoeller
 * @since 1.1
 */
public interface KeyHolder {

  /** 当插入单行数据时指定了返回主键{@link Statement#RETURN_GENERATED_KEYS}时,返回包含主键的列信息,不同数据库返回信息不同 */
  @Nonnull
  Map<String, Object> getKeys() throws TooManyResultException;

  /** 当插入多行数据时指定了返回主键{@link Statement#RETURN_GENERATED_KEYS}时,返回包含主键的列信息,不同数据库返回信息不同 */
  List<Map<String, Object>> getKeyList();
}
