package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.util.List;
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
@ToString(callSuper = true, exclude = "productSpu")
@EqualsAndHashCode(callSuper = true)
public class Product extends AbstractEntityPoJo<Product> {

  private String name;

  @OneToOne private ProductSpu productSpu;

  @OneToMany private List<ProductSku> skuses;
}
