package io.github.ramerf.wind.demo.entity.domain;

import io.github.ramerf.wind.core.annotation.*;
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
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true, exclude = "product")
public class ProductSpu extends AbstractEntityPoJo {

  private String address;

  @OneToOne private Product product;

  @TableColumn(name = "product_id")
  private Long productId;
}
