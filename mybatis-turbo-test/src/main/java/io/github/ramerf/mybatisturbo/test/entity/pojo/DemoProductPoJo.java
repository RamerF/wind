package io.github.ramerf.mybatisturbo.test.entity.pojo;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.mybatisturbo.core.entity.enums.InterEnum;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.BitSet;
import java.util.List;
import javax.persistence.Column;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@TableName(value = "demo_product")
@EqualsAndHashCode(callSuper = true)
@ApiModel("示例商品")
public class DemoProductPoJo extends AbstractEntityPoJo {
  @ApiModelProperty(value = "名称", example = "名称")
  private String name;

  @ApiModelProperty(value = "CODE", example = "CODE")
  private String code;

  @TableField("demo_product_category_id")
  @Column(columnDefinition = "bigint")
  @ApiModelProperty(value = "商品分类id", example = "1")
  private Long demoProductCategoryId;

  @TableField(exist = false)
  private DemoProductCategoryPoJo productCategory;

  @ApiModelProperty(value = "sku id 集合", example = "1,2,3")
  @Column(columnDefinition = "bigint[]")
  private List<Long> skuIds;

  @ApiModelProperty(value = "测试text数组", example = "'名称1','名称2'")
  private String[] texts;

  @ApiModelProperty(value = "测试List<String>", example = "'名称1','名称2'")
  private List<String> stringList;

  @Column(columnDefinition = "bit[]")
  private BitSet bitSet;

  @Column(columnDefinition = "smallint")
  private Type type;

  @Column(columnDefinition = "smallint")
  private State state;

  @Column(columnDefinition = "double")
  private BigDecimal bigDecimal;

  public enum Type implements InterEnum {
    /**
     * 商品类别
     */
    PHONE(0, "手机"), SPORT(1, "运动");

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

  public enum State implements InterEnum {
    /**
     * 商品类别
     */
    UP_SHELVES(0, "上架"), DOWN_SHELVES(1, "下架");

    private final int value;
    private final String desc;

    State(int value, String desc) {
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
