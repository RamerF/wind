package io.github.ramerf.wind.core.entity.pojo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import java.util.Date;
import javax.persistence.*;
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
@MappedSuperclass
public class AbstractEntityPoJo implements AbstractEntity {
  public static final String LOGIC_DELETE_FIELD_NAME = "deleted";
  public static final String LOGIC_DELETE_COLUMN_NAME = "deleted";

  public static final String CREATE_TIME_FIELD_NAME = "createTime";
  public static final String CREATE_TIME_COLUMN_NAME = "create_time";

  public static final String UPDATE_TIME_FIELD_NAME = "updateTime";
  public static final String UPDATE_TIME_COLUMN_NAME = "update_time";

  // 解决字段过长前端显示错误: @JsonSerialize(using = LongJsonSerializer.class)

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** 是否逻辑删除,false:未删除,所有的查询默认支只会查询未删除的数据. */
  @Builder.Default
  @TableColumn(defaultValue = "false")
  private boolean deleted = false;

  /** 创建时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  private Date createTime;

  /** 修改时间 */
  @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
  private Date updateTime;

  /** 所在数据库名称,不能赋值,因为该值始终为数据库默认值:DATABASE(),暂时没用该字段. */
  @Setter(AccessLevel.NONE)
  private static transient String databaseName;
}
