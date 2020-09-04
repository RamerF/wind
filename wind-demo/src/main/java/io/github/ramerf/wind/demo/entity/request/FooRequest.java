package io.github.ramerf.wind.demo.entity.request;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.validation.InterEnumConstraint;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Alphabet;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Type;
import io.swagger.annotations.ApiModel;
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
  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @InterEnumConstraint(message = "类型 值无效", clazz = Type.class, required = true)
  private Foo.Type type;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 varchar */
  @InterEnumConstraint(message = "Alphabet 值无效", clazz = Alphabet.class)
  private Alphabet alphabet;
}
