package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.OneToOne;
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
@ToString(callSuper = true, exclude = "product")
@EqualsAndHashCode(callSuper = true)
public class ProductSpu extends AbstractEntityPoJo {

  private String address;

  /** 单向{@link OneToOne}关联. */
  @OneToOne private Product product;

  private Long productId;

  /** 双向{@link OneToOne}关联,自定义关联字段. */
  @OneToOne(referenceField = "code")
  private ProductSpuCode productSpuCode;
}
