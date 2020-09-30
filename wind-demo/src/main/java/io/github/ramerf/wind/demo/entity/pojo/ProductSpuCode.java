package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020.09.30
 */
@Entity
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class ProductSpuCode extends AbstractEntityPoJo {

  private String code;

  @OneToOne private ProductSpu productSpu;

  @OneToOne private Product product;
}
