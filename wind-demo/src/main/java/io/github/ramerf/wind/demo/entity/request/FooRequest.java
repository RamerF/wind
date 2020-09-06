package io.github.ramerf.wind.demo.entity.request;

import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Alphabet;
import io.swagger.annotations.ApiModel;
import javax.validation.constraints.NotNull;
import lombok.*;

/**
 * Foo.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Data
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
@ApiModel("Foo")
public class FooRequest extends AbstractEntityRequest<Foo> {
  @NotNull(message = "类型值无效")
  private Foo.Type type;

  private Alphabet alphabet;
}
