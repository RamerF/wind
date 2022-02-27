/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.exception.MappingException;
import java.lang.reflect.Type;
import java.util.*;

/**
 * <b>Reference: {@code org.hibernate.dialect.TypeNames}.</b>
 *
 * <p>This class maps a type to names. Associations may be marked with a capacity. Calling the get()
 * method with a type and actual size n will return the associated name with smallest capacity >= n,
 * if available and an unmarked default type otherwise. Eg, setting
 *
 * <pre>
 * names.put( type,        "TEXT" );
 * names.put( type,   255, "VARCHAR($l)" );
 * names.put( type, 65534, "LONGVARCHAR($l)" );
 * </pre>
 *
 * will give you back the following:
 *
 * <pre>
 *  names.get( type )         // --> "TEXT" (default)
 *  names.get( type,    100 ) // --> "VARCHAR(100)" (100 is in [0:255])
 *  names.get( type,   1000 ) // --> "LONGVARCHAR(1000)" (1000 is in [256:65534])
 *  names.get( type, 100000 ) // --> "TEXT" (default)
 * </pre>
 *
 * On the other hand, simply putting
 *
 * <pre>
 * names.put( type, "VARCHAR($l)" );
 * </pre>
 *
 * would result in
 *
 * <pre>
 *  names.get( type )        // --> "VARCHAR($l)" (will cause trouble)
 *  names.get( type, 100 )   // --> "VARCHAR(100)"
 *  names.get( type, 10000 ) // --> "VARCHAR(10000)"
 * </pre>
 *
 * @author Christoph Beck
 */
public class TypeNames {
  /** Holds default type mappings for a typeCode. This is the non-sized mapping */
  private final Map<Type, String> defaults = new HashMap<>();

  /**
   * Holds the weighted mappings for a typeCode. The nested map is a TreeMap to sort its contents
   * based on the key (the weighting) to ensure proper iteration ordering during {@link #get(Type,
   * long, int, int)}
   */
  private final Map<Type, Map<Long, String>> weighted = new HashMap<>();

  /**
   * get default type name for specified type
   *
   * @param type the type key
   * @return the default type name associated with specified key
   * @throws MappingException Indicates that no registrations were made for that type
   */
  public String get(Type type) throws MappingException {
    final String result = defaults.get(type);
    if (result == null) {
      throw new MappingException(
          "No Dialect mapping for JDBC type: "
              + type
              + ",manually specify by @TableColumn#columnDefinition");
    }
    return result;
  }

  /**
   * get type name for specified type and size
   *
   * @param type the type key
   * @param size the SQL length
   * @param scale the SQL scale
   * @param precision the SQL precision
   * @return the associated name with smallest capacity >= size, if available and the default type
   *     name otherwise
   * @throws MappingException Indicates that no registrations were made for that type
   */
  public String get(Type type, long size, int precision, int scale) throws MappingException {
    final Map<Long, String> map = weighted.get(type);
    if (map != null && map.size() > 0) {
      // iterate entries ordered by capacity to find first fit
      for (Map.Entry<Long, String> entry : map.entrySet()) {
        if (size <= entry.getKey()) {
          return replace(entry.getValue(), size, precision, scale);
        }
      }
    }

    // if we get here one of 2 things happened:
    //		1) There was no weighted registration for that type
    //		2) There was no weighting whose max capacity was big enough to contain size
    return replace(get(type), size, precision, scale);
  }

  private static String replace(String type, long size, int precision, int scale) {
    type = replaceOnce(type, "$s", Integer.toString(scale));
    type = replaceOnce(type, "$l", Long.toString(size));
    return replaceOnce(type, "$p", Integer.toString(precision));
  }

  /**
   * Register a weighted typeCode mapping
   *
   * @param typeCode the JDBC type code
   * @param capacity The capacity for this weighting
   * @param value The mapping (type name)
   */
  public void put(Type typeCode, long capacity, String value) {
    weighted.computeIfAbsent(typeCode, k -> new TreeMap<>()).put(capacity, value);
  }

  /**
   * Register a default (non-weighted) typeCode mapping
   *
   * @param typeCode the type key
   * @param value The mapping (type name)
   */
  public void put(Type typeCode, String value) {
    defaults.put(typeCode, value);
  }

  /**
   * Check whether or not the provided typeName exists.
   *
   * @param typeName the type name.
   * @return true if the given string has been registered as a type.
   */
  public boolean containsTypeName(final String typeName) {
    return this.defaults.containsValue(typeName);
  }

  public static String replaceOnce(String template, String placeholder, String replacement) {
    if (template == null) {
      return null; // returnign null!
    }
    int loc = template.indexOf(placeholder);
    if (loc < 0) {
      return template;
    } else {
      return template.substring(0, loc)
          + replacement
          + template.substring(loc + placeholder.length());
    }
  }
}
