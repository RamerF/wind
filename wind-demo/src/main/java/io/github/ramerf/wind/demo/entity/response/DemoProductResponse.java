package io.github.ramerf.wind.demo.entity.response;

import io.github.ramerf.wind.core.entity.response.AbstractEntityResponse;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo.State;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo.Type;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.*;
import lombok.*;
import org.springframework.beans.BeanUtils;

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
@ApiModel(value = "商品")
public class DemoProductResponse extends AbstractEntityResponse {

  @ApiModelProperty(value = "String", example = "示例值")
  private String name;

  @ApiModelProperty(value = "Long", example = "1")
  private Long demoProductCategoryId;

  @ApiModelProperty(value = "List", example = "示例值")
  private List<Long> skuIds;

  @ApiModelProperty(value = "productCategory", example = "1")
  private Long productCategoryId;

  @ApiModelProperty(value = "测试text数组", example = "'名称1','名称2'")
  private String[] texts;

  private BitSet bitSet;

  private Type type;

  private State state;

  public static DemoProductResponse of(final DemoProductPoJo product) {
    if (Objects.isNull(product)) {
      return null;
    }
    DemoProductResponse poJo = new DemoProductResponse();
    // TODO-WARN: 将 Domain 对象转换成 Response 对象
    BeanUtils.copyProperties(product, poJo);
    return poJo;
  }
}
