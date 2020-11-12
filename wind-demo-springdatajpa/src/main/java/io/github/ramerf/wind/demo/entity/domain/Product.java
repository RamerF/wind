package io.github.ramerf.wind.demo.entity.domain;

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
@ToString(callSuper = true, exclude = "productSpu")
@EqualsAndHashCode(callSuper = true)
public class Product extends AbstractEntityPoJo {

  private String name;

  @OneToOne private ProductSpu productSpu;
}
