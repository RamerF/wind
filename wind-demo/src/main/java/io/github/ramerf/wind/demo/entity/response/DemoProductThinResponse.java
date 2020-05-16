package io.github.ramerf.wind.demo.entity.response;

import io.github.ramerf.wind.core.entity.response.AbstractEntityResponse;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * 商品.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ApiModel(value = "商品只包含部分属性")
public class DemoProductThinResponse extends AbstractEntityResponse {

  @ApiModelProperty(value = "String", example = "示例值")
  private String name;
}
