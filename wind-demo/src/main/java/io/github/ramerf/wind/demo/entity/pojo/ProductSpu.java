package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.OneToOne;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.Domain;
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
public class ProductSpu extends Domain<ProductSpu, String> {
  @Id private String id;
  private String address;

  /** 单向{@link OneToOne}关联. */
  @OneToOne private Product product;

  private String productId;

  /** 双向{@link OneToOne}关联,自定义关联字段. */
  @OneToOne(targetField = "code")
  private ProductSpuCode productSpuCode;
}
