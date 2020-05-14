package io.github.ramerf.wind.core.converter;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class StringArrayTypeConverter implements TypeConverter<String[], String[]> {
  @Override
  public String[] convertToJdbc(String[] javaVal) {
    return javaVal;
  }

  @Override
  public String[] covertFromJdbc(final String[] jdbcVal, final Class<? extends String[]> clazz) {
    return jdbcVal;
  }
}
