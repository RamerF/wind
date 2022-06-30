package io.github.ramerf.wind.demo.entity;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.annotation.TableIndex.Index;
import io.github.ramerf.wind.core.annotation.TableIndex.IndexField;
import java.time.LocalDateTime;
import javax.persistence.Id;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * The type Foo.
 *
 * @since 2022.04.14
 * @author ramer
 */
@Data
@TableInfo(comment = "示例表")
@TableIndex({
  @Index(indexFields = @IndexField(field = "deleted")),
  @Index(indexFields = @IndexField(field = "boolValue")),
})
@EqualsAndHashCode
public class Foo {
  @Id
  @TableColumn(comment = "主键")
  private Long id;

  private boolean boolValue;

  @TableColumn(comment = "名称")
  private String name;

  @TableColumn(comment = "创建时间")
  @CreateTimestamp
  private LocalDateTime createTime;

  @TableColumn(comment = "删除状态.true:已删除")
  private boolean deleted;

  @TableColumn(comment = "最后更新时间")
  @UpdateTimestamp
  private LocalDateTime updateTime;
}
