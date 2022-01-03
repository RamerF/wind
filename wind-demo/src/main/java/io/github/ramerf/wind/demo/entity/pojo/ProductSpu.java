package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
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
@ToString(callSuper = true, exclude = "product")
@EqualsAndHashCode(callSuper = true)
public class ProductSpu extends Domain<ProductSpu, String> {
  @Id private String id;
  private String address;

  /** 单向{@link OneToOne}关联. */
  @OneToOne private Product product;

  private String productId;

  /** 双向{@link OneToOne}关联,自定义关联字段. */
  @OneToOne(targetField = "code", joinField = "spuCode")
  private ProductSpuCode productSpuCode;

  private long spuCode;
}
