package io.github.ramerf.mybatisturbo.core.entity.pojo;

import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import java.util.Date;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * PoJo实体.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/6
 */
@Slf4j
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class AbstractEntityPoJo implements AbstractEntity {
  public static final String COLUMN_ID = "id";
  public static final String COLUMN_COMPANY_ID = "company_id";

  // @JsonSerialize(using = LongJsonSerializer.class)

  private Long id;

  private Long companyId;
  private Long createId;
  private Long updateId;
  @Builder.Default @TableLogic private Boolean isDelete = Boolean.FALSE;

  /** 创建时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  @TableField(fill = FieldFill.INSERT)
  private Date createTime;

  /** 修改时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  @TableField(fill = FieldFill.INSERT_UPDATE)
  private Date updateTime;

  // TODO-WARN 临时取消字段
  /** 数据源名称 */
  //  @TableField(
  //      insertStrategy = FieldStrategy.NEVER,
  //      updateStrategy = FieldStrategy.NEVER,
  //      select = false)
  //  private String databaseName;

}
