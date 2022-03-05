package io.github.ramerf.wind.core.pgsql;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.annotation.TableIndex.Index;
import io.github.ramerf.wind.core.annotation.TableIndex.IndexField;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.domain.Domain;
import io.github.ramerf.wind.core.domain.InterEnum;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import javax.persistence.Id;
import lombok.*;

/**
 * @author ramer
 * @since 2019/12/16
 */
@TableInfo(
    name = "foo",
    comment = "测试表",
    logicDelete = @LogicDelete(enable = true, fieldName = "hasDeleted"))
@TableIndex({
  @Index(
      name = "idx_foo_name",
      indexFields = {@IndexField(field = "name")}),
  @Index(
      name = "idx_foo_name_age",
      indexFields = {
        @IndexField(field = "name"),
        @IndexField(field = "age", direction = Direction.DESC)
      })
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Foo extends Domain<Foo, Long> {
  @Id private Long id;
  private String name;

  /** 基本类型. */
  private int age;

  @TableColumn(columnDefinition = "text")
  private String textString;

  /** List&lt;Long&gt; 可对应数据库类型 bigint[] */
  @TableColumn(columnDefinition = "bigint[]")
  private List<Long> longList;

  /** Long[]可对应数据库类型 bigint[] */
  private Long[] longArr;

  /** List&lt;String&gt; 可对应数据库类型 text[]/varchar[],可指定集合类型 */
  private List<String> stringList = new ArrayList<>();

  /** String[] 可对应数据库类型 text[]/varchar[] */
  private String[] stringArr;

  /** Bitset 可对应数据库类型 bytea */
  @TableColumn(columnDefinition = "bytea")
  @TypeHandler(BitSetByteArrTypeHandler.class)
  private BitSet bitSet;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @TableColumn(columnDefinition = "smallint")
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

  /** 自定义逻辑删除字段. */
  @TableColumn(defaultValue = "false")
  private boolean hasDeleted;

  /** 大文本字段,测试{@link Fields} include/exclude. */
  @TableColumn(comment = "大文本字段,测试Fields include/exclude", columnDefinition = "text")
  private String bigText;

  /* boolean类型. */
  private boolean isNumber;
  private boolean string;
  private Boolean isNull;
  private Boolean nonNull;

  @TableColumn(columnDefinition = "text", comment = "对象集合转存json,可指定集合类型")
  @TypeHandler(ObjectCollectionToJsonTypeHandler.class)
  private List<Type> typesJson = new LinkedList<>();

  @TableColumn(columnDefinition = "text", comment = "对象转存json")
  @TypeHandler(ObjectToJsonTypeHandler.class)
  private Type typeJson;

  @CreateTimestamp private LocalDateTime createTime;
  @UpdateTimestamp private LocalDateTime updateTime;

  @OneToOne private FooSub fooSub;
  private String fooSubNames;

  public enum Type implements InterEnum<Integer> {
    /** 商品类别 */
    PHONE(0, "手机"),
    SPORT(1, "运动");

    private final int value;
    private final String desc;

    Type(int value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public Integer value() {
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

  @TableInfo
  public static class FooSub extends Domain<io.github.ramerf.wind.core.mysql.Foo.FooSub, String> {
    @Id private String name;
    @CreateTimestamp private LocalDateTime createTime;
  }
}
