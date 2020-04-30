package io.github.ramerf.mybatisturbo.test.entity.pojo;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ApiModel("示例商品")
@TableName(value = "demo_product_category")
public class DemoProductCategoryPoJo extends AbstractEntityPoJo {
  @ApiModelProperty(value = "名称", example = "名称")
  private String name;
}
