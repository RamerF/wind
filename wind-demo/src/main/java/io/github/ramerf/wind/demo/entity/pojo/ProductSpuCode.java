package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import javax.persistence.Id;
import lombok.*;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020.09.30
 */
@TableInfo
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class ProductSpuCode extends AbstractEntityPoJo<ProductSpuCode, Long> {
  @Id private String id;

  @TableColumn(defaultValue = "0")
  private long code;

  @OneToOne(field = "code", joinColumn = false)
  private ProductSpu productSpu;

  @OneToOne private Product product;
}
