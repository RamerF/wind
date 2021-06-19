package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.Domain;
import javax.persistence.Id;
import lombok.*;

/**
 * .
 *
 * @author ramer
 * @since 2020.09.30
 */
@TableInfo(logicDelete = @LogicDelete(enable = false))
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class ProductSpuCode extends Domain<ProductSpuCode, Long> {
  @Id private String id;

  @TableColumn(defaultValue = "0")
  private long code;

  @OneToOne(targetField = "productSpuCode", shouldJoinColumn = false)
  private ProductSpu productSpu;

  @OneToOne private Product product;
}
