package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.ManyToOne;
import io.github.ramerf.wind.core.annotation.TableInfo;
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
@EqualsAndHashCode(callSuper = true)
public class ProductSkuSupplier extends AbstractEntityPoJo {
  private String name;

  /** 单向{@link ManyToOne}. */
  @ManyToOne private ProductSku productSku;
}
