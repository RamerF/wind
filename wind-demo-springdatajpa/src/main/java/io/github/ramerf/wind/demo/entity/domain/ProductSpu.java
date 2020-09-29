package io.github.ramerf.wind.demo.entity.domain;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import javax.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author ramer
 * @since 12/09/2020
 */
@Entity
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true, exclude = "product")
public class ProductSpu extends AbstractEntityPoJo {

  private String address;

  @OneToOne
  @JoinColumn(name = "product_id")
  private Product product;

  @Column(name = "product_id", insertable = false, updatable = false)
  private Long productId;
}
