package io.github.ramerf.wind.demo.entity.request;

import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo.State;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo.Type;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.List;
import lombok.*;
import org.hibernate.validator.constraints.Length;

/**
 * 商品.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
@ApiModel("商品")
public class DemoProductRequest extends AbstractEntityRequest<DemoProductPoJo> {
  public static final int NAME_MAX_LENGTH = 50;

  @Length(max = 50)
  @ApiModelProperty(value = "String", example = "示例值")
  private String name;

  @ApiModelProperty(value = "Long", example = "1")
  private Long demoProductCategoryId;

  @ApiModelProperty(value = "List", example = "示例值")
  private List<Long> skuIds;

  @ApiModelProperty(value = "productCategory", example = "1")
  private Long productCategoryId;

  private Type type;

  private State state;
}
