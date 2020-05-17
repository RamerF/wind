package io.github.ramerf.wind.core.entity.request;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.swagger.annotations.ApiModelProperty;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import static io.github.ramerf.wind.core.util.BeanUtils.initial;

/**
 * API请求抽象类.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /12/6
 */
@Slf4j
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public abstract class AbstractEntityRequest<T extends AbstractEntityPoJo>
    implements AbstractEntity {

  @ApiModelProperty(value = "主键ID", example = "235455")
  private Long id;

  @ApiModelProperty(value = "修改人ID既当前操作人ID", example = "3235455")
  private Long updateId;

  @ApiModelProperty(value = "创建ID既当前操作人ID", example = "3235455")
  private Long createId;

  /**
   * Request实体转换为Domain实体的额外处理.
   *
   * @param domain Domain实体 {@link AbstractEntityPoJo}.
   */
  @SuppressWarnings({"unused"})
  public final void redundantValue(T domain) {}

  /**
   * 获取Request实体对应的的PoJo对象. <br>
   * 注意: 使用该方法,需要Request继承的AbstractEntity具有PoJo泛型.<br>
   * 例如: public class DemoProductRequest extends AbstractEntityRequest&lt;DemoProductPoJo&gt;
   *
   * @return the t
   */
  @SuppressWarnings({"unchecked"})
  public T poJo() {
    final Type genericSuperclass = this.getClass().getGenericSuperclass();
    if (!(genericSuperclass instanceof ParameterizedType)) {
      throw CommonException.of("无法获取pojo对象,请修改request类,添加pojo泛型");
    }
    final Object poJo =
        initial(((ParameterizedType) genericSuperclass).getActualTypeArguments()[0].getTypeName());
    BeanUtils.copyProperties(this, poJo);
    return (T) poJo;
  }
}
