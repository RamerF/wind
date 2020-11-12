package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.annotation.OneToMany;
import java.util.List;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2020/07/24
 */
@TableInfo(name = "organize", comment = "the organize")
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Organize extends AbstractEntityPoJo<Organize> {
  private String name;

  @OneToMany private List<Department> departments;
}
