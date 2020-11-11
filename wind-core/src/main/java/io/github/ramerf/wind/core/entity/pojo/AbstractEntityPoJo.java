package io.github.ramerf.wind.core.entity.pojo;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.service.GenericService;
import java.util.Date;
import java.util.List;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import javax.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
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
public class AbstractEntityPoJo<T extends AbstractEntityPoJo<?>> implements AbstractEntity {
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
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  public final long create() throws DataAccessException {
    genericService().create(instance());
    return getId();
  }

  /**
   * 创建记录.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  public final long createWithNull(List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
    genericService().createWithNull(instance(), includeNullProps);
    return getId();
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update() throws DataAccessException {
    return genericService().update(instance());
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param includeNullProps 即使值为null也保存的属性
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int updateWithNull(List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
    return genericService().updateWithNull(instance(), includeNullProps);
  }

  /**
   * 条件更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param consumer 更新条件
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Consumer<Condition<T>> consumer) throws DataAccessException {
    return consumer == null
        ? genericService().update(instance())
        : genericService().getUpdate().lambdaWhere(consumer).update(instance());
  }

  /**
   * 条件更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param consumer 更新条件
   * @param includeNullProps 即使值为null也保存的属性
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int updateWithNull(
      @Nonnull final Consumer<Condition<T>> consumer, List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
    return genericService()
        .getUpdate()
        .lambdaWhere(consumer)
        .updateWithNull(instance(), includeNullProps);
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  @SuppressWarnings("unchecked")
  @JsonIgnore
  @JSONField(serialize = false, deserialize = false)
  private T instance() {
    return (T) this;
  }

  @SuppressWarnings("unchecked")
  private GenericService<T> genericService() {
    return GenericService.with((Class<T>) instance().getClass());
  }
}
