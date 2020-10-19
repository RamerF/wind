package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.util.List;
import javax.persistence.ManyToMany;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2020/07/24
 */
@TableInfo(name = "t_role", comment = "the role")
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Department extends AbstractEntityPoJo {
  private String name;

  @ManyToMany private List<Account> accounts;
}
