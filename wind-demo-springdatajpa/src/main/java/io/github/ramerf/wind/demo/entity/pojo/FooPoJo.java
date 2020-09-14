package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.math.BigDecimal;
import java.util.BitSet;
import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@Entity(name = "foo")
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class FooPoJo extends AbstractEntityPoJo {
  private String name;

  @Column(columnDefinition = "text")
  private String textString;

  /** List&lt;Long&gt; 可对应数据库类型 bigint[] */
  @Column(columnDefinition = "bigint[]")
  private List<Long> longList;

  /** Long[]可对应数据库类型 bigint[] */
  private Long[] longArr;

  /** List&lt;String&gt; 可对应数据库类型 text[]/varchar[] */
  private List<String> stringList;

  /** String[] 可对应数据库类型 text[]/varchar[] */
  private String[] stringArr;

  /** Bitset 可对应数据库类型 bytea */
  @Column(columnDefinition = "bytea")
  private BitSet bitSet;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @Column(columnDefinition = "smallint")
  private Type type;

  @Column(columnDefinition = "numeric(5,2)")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link Column#name()}指定数据库字段名. */
  @Column(name = "non_match_column")
  private String column;

  /** List&lt;Integer&gt; 可对应数据库类型 int[] */
  private List<Integer> intList;

  /** Integer[]可对应数据库类型 int[] */
  private Integer[] intArr;

  public enum Type implements InterEnum {
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
}
