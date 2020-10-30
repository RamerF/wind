package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.OneToOne;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author ramer
 * @since 12/09/2020
 */
@TableInfo
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true, exclude = "product")
@EqualsAndHashCode(callSuper = true)
public class ProductSpu extends AbstractEntityPoJo {

  private String address;

  @OneToOne private Product product;

  private Long productId;

  @OneToOne(referenceField = "code")
  private ProductSpuCode productSpuCode;
}
