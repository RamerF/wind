package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.OneToOne;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020.09.30
 */
@TableInfo
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class ProductSpuCode extends AbstractEntityPoJo {
  private long code;

  @OneToOne(field = "code")
  private ProductSpu productSpu;

  @OneToOne private Product product;
}
