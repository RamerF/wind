package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.math.BigDecimal;
import java.util.BitSet;
import java.util.List;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@TableInfo(name = "o_o")
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class OoO extends AbstractEntityPoJo<OoO> {
  private String name;

  @TableColumn(columnDefinition = "text")
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
  @TableColumn(columnDefinition = "smallint")
  private Type type;

  @TableColumn(columnDefinition = "numeric(5,2)")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link TableColumn#name()}指定数据库字段名. */
  @TableColumn(name = "non_match_column")
  private String column;

  /** List&lt;Integer&gt; 可对应数据库类型 int[] */
  private List<Integer> intList;

  /** Integer[]可对应数据库类型 int[] */
  private Integer[] intArr;

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
}
