package io.github.ramerf.wind.demo.entity.response;

import io.github.ramerf.wind.core.entity.response.AbstractEntityResponse;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Type;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.*;
import lombok.*;
import org.springframework.beans.BeanUtils;

/**
 * Foo.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ApiModel(value = "foo")
public class FooResponse extends AbstractEntityResponse {

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

  public static FooResponse of(final Foo foo) {
    if (Objects.isNull(foo)) {
      return null;
    }
    FooResponse poJo = new FooResponse();
    BeanUtils.copyProperties(foo, poJo);
    return poJo;
  }
}
