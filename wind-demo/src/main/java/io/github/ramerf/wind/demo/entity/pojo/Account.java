package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.LongTimestampTypeHandler;
import java.util.Date;
import javax.persistence.Column;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2020/07/24
 */
@TableInfo(name = "account", logicDelete = @LogicDelete(column = "has_deleted"))
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Account extends AbstractEntityPoJo {

  private String name;

  private String tel;

  /** 自定义逻辑删除字段. */
  private Boolean hasDeleted;

  /** 自定义创建时间. */
  @CreateTimestamp
  @TypeHandler(LongTimestampTypeHandler.class)
  private long createDate;

  /** 自定义更新时间. */
  @Column(columnDefinition = "timestamp with time zone")
  @UpdateTimestamp
  private Date updateDate;
}
