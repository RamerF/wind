package io.github.ramerf.wind.demo.entity.request;

import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Type;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import org.hibernate.validator.constraints.Length;

/**
 * Foo.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
@ApiModel("Foo")
public class FooRequest extends AbstractEntityRequest<Foo> {
  public static final int NAME_MAX_LENGTH = 50;

  @Length(max = 50)
  @ApiModelProperty(value = "String", example = "示例值")
  private String name;

  private Type type;
}
