package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel("Foo")
public class Foo extends AbstractEntityPoJo {
  @ApiModelProperty(value = "名称", example = "名称")
  private String name;

  @ApiModelProperty(value = "CODE", example = "CODE")
  private String code;

  /** List&lt;Long&gt; 可对应数据库类型 bigint[] */
  @ApiModelProperty(value = "sku id 集合", example = "1,2,3")
  @Column(columnDefinition = "bigint[]")
  private List<Long> skuIds;

  /** String[] 可对应数据库类型 text[]/varchar[] */
  @ApiModelProperty(value = "测试text数组", example = "'名称1','名称2'")
  private String[] texts;

  @ApiModelProperty(value = "测试List<String>", example = "'名称1','名称2'")
  private List<String> stringList;

  @Column(columnDefinition = "bit[]")
  private BitSet bitSet;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @Column(columnDefinition = "smallint")
  private Type type;

  @Column(columnDefinition = "double")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link Column#name()}指定数据库字段名. */
  @Column(name = "non_match_column")
  private String column;

  private List<Integer> intArr;

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
