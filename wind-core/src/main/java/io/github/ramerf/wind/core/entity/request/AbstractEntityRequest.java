package io.github.ramerf.wind.core.entity.request;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;

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
public abstract class AbstractEntityRequest<
        T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
    implements AbstractEntity {

  @ApiModelProperty(value = "主键ID", example = "235455")
  private ID id;

  /**
   * Request实体转换为Domain实体的额外处理,比如敏感词过滤.
   *
   * @param domain Domain实体 {@link AbstractEntityPoJo}.
   */
  public void redundantValue(T domain) {}

  /**
   * 获取Request实体对应的的PoJo对象. <br>
   * 注意: 使用该方法,需要<code>request</code>对象指定<code>pojo</code>泛型.<br>
   * 例如: public class FooRequest extends AbstractEntityRequest&lt;FooPoJo&gt;
   *
   * @return the t
   */
  public final T poJo() {
    return poJo(null);
  }

  /**
   * 获取Request实体对应的的PoJo对象. <br>
   * 注意: 使用该方法,需要<code>request</code>对象指定<code>pojo</code>泛型.<br>
   * 例如: public class FooRequest extends AbstractEntityRequest&lt;FooPoJo&gt;
   *
   * @return the t
   */
  public final T poJo(final ID id) {
    final Type genericSuperclass = this.getClass().getGenericSuperclass();
    if (!(genericSuperclass instanceof ParameterizedType)) {
      throw CommonException.of("无法获取pojo对象,请修改request类,添加pojo泛型");
    }
    final T poJo =
        initial(((ParameterizedType) genericSuperclass).getActualTypeArguments()[0].getTypeName());
    BeanUtils.copyProperties(this, poJo);
    if (id != null) {
      @SuppressWarnings("unchecked")
      final Class<T> clazz = (Class<T>) poJo.getClass();
      BeanUtils.setValue(poJo, EntityHelper.getEntityIdField(clazz), id, null);
    }
    return poJo;
  }

  /**
   * 获取PoJo的class对象.<br>
   * 注意: 使用该方法,需要<code>request</code>对象指定<code>pojo</code>泛型.<br>
   */
  public final Class<T> poJoClass() {
    final Type genericSuperclass = this.getClass().getGenericSuperclass();
    if (!(genericSuperclass instanceof ParameterizedType)) {
      throw CommonException.of("无法获取pojo对象,请修改request类,添加pojo泛型");
    }
    return BeanUtils.getClazz(
        ((ParameterizedType) genericSuperclass).getActualTypeArguments()[0].getTypeName());
  }
}
