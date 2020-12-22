package io.github.ramerf.wind.demo.entity.pojo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.service.InterService.Fields;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.*;
import java.util.Date;
import java.util.*;
import javax.annotation.Nonnull;
import javax.persistence.*;
import lombok.*;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@TableInfo(name = "foo", comment = "the foo.")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Foo extends AbstractEntityPoJo<Foo, Long> {

  // 解决字段过长前端显示错误: @JsonSerialize(using = LongJsonSerializer.class)
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** 是否逻辑删除,false:未删除,所有的查询默认只会查询未删除的数据. */
  @Builder.Default
  @TableColumn(defaultValue = "false")
  private boolean deleted = false;

  /** 创建时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  @CreateTimestamp
  private Date createTime;

  /** 修改时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  @UpdateTimestamp
  private Date updateTime;

  private String name;

  /** 基本类型. */
  private int age;

  @TableColumn(comment = "text string comment", columnDefinition = "text")
  private String textString;

  /** List&lt;Long&gt; 可对应数据库类型 bigint[] */
  @TableColumn(columnDefinition = "bigint[]")
  private List<Long> longList;

  /** Long[]可对应数据库类型 bigint[] */
  private Long[] longArr;

  /** List&lt;String&gt; 可对应数据库类型 text[]/varchar[] */
  private List<String> stringList;

  /** String[] 可对应数据库类型 text[]/varchar[] */
  private String[] stringArr;

  /** Bitset 可对应数据库类型 bytea */
  @TableColumn(columnDefinition = "bytea")
  private BitSet bitSet;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  // @TableColumn(columnDefinition = "smallint")
  private Type type;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 varchar */
  @TableColumn(columnDefinition = "varchar(1)")
  private Alphabet alphabet;

  @TableColumn(columnDefinition = "numeric(5,2)")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link TableColumn#name()}指定数据库字段名. */
  @TableColumn(name = "non_match_column")
  private String column;

  /** List&lt;Integer&gt; 可对应数据库类型 int[] */
  private List<Integer> intList;

  /** Integer[]可对应数据库类型 int[] */
  private Integer[] intArr;

  /** 大文本字段,测试{@link Fields} include/exclude. */
  @TableColumn(comment = "大文本字段,测试Fields include/exclude", columnDefinition = "text")
  private String bigText;

  /* boolean类型. */
  private boolean isNumber;
  private boolean string;
  private Boolean isNull;
  private Boolean nonNull;

  /** 自定义类型转换器. */
  @TypeHandler(SetTypeHandler.class)
  private Set<Long> noDuplicateIds = new HashSet<>();

  public enum Type implements InterEnum<String> {
    /** Type. */
    PHONE("0", "手机"),
    SPORT("1", "运动");

    private final String value;
    private final String desc;

    Type(String value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public String value() {
      return this.value;
    }

    @Override
    public String desc() {
      return this.desc;
    }
  }

  public enum Alphabet implements InterEnum<String> {
    /** Alphabet. */
    _A("A", "A"),
    _B("B", "B");

    private final String value;
    private final String desc;

    Alphabet(String value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public String value() {
      return this.value;
    }

    @Override
    public String desc() {
      return this.desc;
    }
  }

  public static class SetTypeHandler implements ITypeHandler<Set<Long>, Long[]> {
    @Override
    public Object convertToJdbc(
        Set<Long> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
      if (javaVal == null) {
        return null;
      }
      try {
        final Connection connection = ps.getConnection();
        return connection.createArrayOf(getJdbcType(field), javaVal.toArray(new Long[0]));
      } catch (SQLException e) {
        throw CommonException.of(e);
      }
    }

    @Override
    public Set<Long> covertFromJdbc(final Long[] jdbcVal, final Class<? extends Set<Long>> clazz) {
      if (jdbcVal == null) {
        return new HashSet<>();
      }
      Set<Long> set = new HashSet<>();
      Collections.addAll(set, jdbcVal);
      return set;
    }

    @Override
    public String getJdbcType(@Nonnull final Field field) {
      return EntityHelper.getJdbcTypeName(field, "bigint");
    }
  }
}
