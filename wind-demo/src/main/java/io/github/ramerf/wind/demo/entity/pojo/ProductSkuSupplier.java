package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.Domain;
import javax.persistence.Id;
import lombok.*;

/**
 * @author ramer
 * @since 12/09/2020
 */
@TableInfo(logicDelete = @LogicDelete(enable = false))
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class ProductSkuSupplier extends Domain<ProductSkuSupplier, String> {
  @Id private String id;
  private String name;

  /** 单向{@link ManyToOne}. */
  @ManyToOne private ProductSku productSku;

  private Long productSkuId;
}
