package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.ManyToOne;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import javax.persistence.Id;
import lombok.*;

/**
 * @author ramer
 * @since 12/09/2020
 */
@TableInfo
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true, exclude = "product")
@EqualsAndHashCode(callSuper = true)
public class ProductSku extends AbstractEntityPoJo<ProductSku, String> {
  @Id private String id;
  private String name;
  @ManyToOne private Product product;
}
