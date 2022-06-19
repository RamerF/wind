package io.github.ramerf.wind.demo.entity.pojo;

import com.alibaba.fastjson.annotation.JSONField;
import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.annotation.TableIndex.Index;
import io.github.ramerf.wind.core.annotation.TableIndex.IndexField;
import java.time.LocalDateTime;
import javax.persistence.Id;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * The type Demo.
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

  @JSONField(format = "yyyy-MM-dd HH:mm:ss")
  // @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @TableColumn(comment = "创建时间")
  @CreateTimestamp
  private LocalDateTime createTime;

  @TableColumn(comment = "删除状态.true:已删除")
  private boolean deleted;

  @JSONField(format = "yyyy-MM-dd HH:mm:ss")
  // @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @TableColumn(comment = "最后更新时间")
  @UpdateTimestamp
  private LocalDateTime updateTime;

  private boolean boolValue;

  @TableColumn(comment = "姓名")
  private String name;
}
