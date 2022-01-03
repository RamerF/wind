package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.LogicDelete;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.Domain;
import io.github.ramerf.wind.core.support.UUIDGenerator;
import javax.persistence.Id;
import lombok.*;

/**
 * @author ramer
 * @since 12/09/2020
 */
@TableInfo(logicDelete = @LogicDelete(enable = false), idGenerator = UUIDGenerator.class)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
// @ToString(callSuper = true, exclude = "product")
@EqualsAndHashCode(callSuper = true)
public class ProductSku extends Domain<ProductSku, String> {
  @Id private String id;
  private String name;
  // @ManyToOne private Product product;
  private String productId;
}
