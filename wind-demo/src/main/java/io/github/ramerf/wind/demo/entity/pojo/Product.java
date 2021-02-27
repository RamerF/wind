package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.Domain;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
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
public class Product extends Domain<Product, String> {
  @Id private String id;
  private String name;
  private String title;
  private Type type;
  private LocalDate localDate;
  private Date date;
  @CreateTimestamp private LocalDateTime createTime;
  @UpdateTimestamp private LocalDateTime updateTime;

  @TableColumn(defaultValue = "false")
  private boolean deleted;
  /** 双向. */
  @OneToMany private List<ProductSku> skuses;

  public enum Type implements InterEnum<Integer> {
    VIRTUAL(1, "VIRTUAL"),
    REALITY(2, "REALITY");
    private final Integer value;
    private final String desc;

    Type(final Integer value, final String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public Integer value() {
      return value;
    }

    @Override
    public String desc() {
      return desc;
    }
  }
}
