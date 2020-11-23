package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.time.LocalDateTime;
import java.util.List;
import javax.persistence.Id;
import lombok.*;

/**
 * 测试string类型的id.
 *
 * @author ramer
 * @since 12/09/2020
 */
@TableInfo(logicDelete = @LogicDelete(enable = true, fieldName = "deleted"))
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class Product extends AbstractEntityPoJo<Product, String> {
  @Id private String id;
  private String name;
  private String title;
  @CreateTimestamp private LocalDateTime createTime;
  @UpdateTimestamp private LocalDateTime updateTime;

  @TableColumn(defaultValue = "false")
  private boolean deleted;
  /** 双向. */
  @OneToMany private List<ProductSku> skuses;
}
