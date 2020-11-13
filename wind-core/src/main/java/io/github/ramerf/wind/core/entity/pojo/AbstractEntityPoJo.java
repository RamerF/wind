package io.github.ramerf.wind.core.entity.pojo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.service.UpdateService.Fields;
import java.util.Date;
import java.util.function.Consumer;
import javax.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * PoJo实体.
 *
 * @since 2019 /12/6
 * @author Tang Xiaofeng
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

  /** 是否逻辑删除,false:未删除,所有的查询默认只会查询未删除的数据. */
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

  /**
   * 创建记录.
   *
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  public final long create() throws DataAccessException {
    genericService().create(instance());
    return getId();
  }

  /**
   * 创建记录.
   *
   * @param <T> the type parameter
   * @param fieldsConsumer the fields consumer
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  public final <T extends AbstractEntityPoJo> long create(final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    return genericService().create(instance(), fieldsConsumer);
  }

  /**
   * 更新.
   *
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  public final int update() throws DataAccessException {
    return genericService().update(instance());
  }

  /**
   * 更新.
   *
   * @param <T> the type parameter
   * @param fieldsConsumer the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  public final <T extends AbstractEntityPoJo> int update(final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    return genericService().update(instance(), fieldsConsumer, null);
  }

  /**
   * 更新.
   *
   * @param <T> the type parameter
   * @param conditionConsumer the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  public final <T extends AbstractEntityPoJo> int updateByCondition(
      final Consumer<Condition<T>> conditionConsumer) throws DataAccessException {
    return genericService().update(instance(), null, conditionConsumer);
  }

  /**
   * 删除记录.
   *
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   * @see CommonException
   */
  public int delete() throws DataAccessException {
    return genericService().delete(id);
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.示例:<br>
   *     {@code condition -> condition.eq(AbstractEntityPoJo::setId, 1L)}
   * @return 删除记录数 long
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntityPoJo> int delete(Consumer<Condition<T>> consumer)
      throws DataAccessException {
    return genericService().delete(consumer);
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  @SuppressWarnings("unchecked")
  private <T extends AbstractEntityPoJo> T instance() {
    return (T) this;
  }

  @SuppressWarnings({"rawtypes"})
  private GenericService genericService() {
    return GenericService.with(instance().getClass());
  }
}
