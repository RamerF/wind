package io.github.ramerf.wind.core.entity.response;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import java.util.Date;
import lombok.*;

/**
 * 请求响应抽象类.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/6
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public abstract class AbstractEntityResponse implements AbstractEntity {
  //  @JsonSerialize(using = LongJsonSerializer.class)

  private Long id;

  private Date createTime;

  private Date updateTime;
}
