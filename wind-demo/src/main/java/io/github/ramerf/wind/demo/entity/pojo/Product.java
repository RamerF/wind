package io.github.ramerf.wind.demo.entity.pojo;

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
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true, exclude = "productSpu")
@EqualsAndHashCode(callSuper = true)
public class Product extends AbstractEntityPoJo {

  private String name;

  @OneToOne @JoinColumn private ProductSpu productSpu;

  @OneToMany private List<ProductSku> skuses;
}
