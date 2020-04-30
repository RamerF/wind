package io.github.ramerf.mybatisturbo.test.entity.request;

import io.github.ramerf.mybatisturbo.core.entity.request.AbstractEntityRequest;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import javax.validation.constraints.NotEmpty;
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
@ApiModel("商品查询请求对象")
public class DemoProductQueryRequest extends AbstractEntityRequest {

  //  @Length(min = 1, max = 5, message = "名称 长度在1-5之间")
  @NotEmpty(message = "名称 不能为空")
  @ApiModelProperty(value = "String", example = "示例值")
  private String name;
}
