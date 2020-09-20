package io.github.ramerf.wind.demo.entity.domain;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.util.List;
import javax.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author ramer
 * @since 12/09/2020
 */
@Entity
// @Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Product extends AbstractEntityPoJo {

  private String name;

  @OneToOne
  @JoinColumn(referencedColumnName = "product_id")
  private ProductSpu productSpu;

  @Setter @ManyToMany @JoinTable private List<ProductCategory> categories;

  public List<ProductCategory> getCategories() {
    return categories;
  }
}
