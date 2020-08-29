package io.github.ramerf.wind.core.pgsql.entity.response;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import lombok.Getter;
import lombok.Setter;

/**
 * @author Tang Xiaofeng
 * @since 2020/8/5
 */
@Getter
@Setter
public class IdNameResponse implements AbstractEntity {
  private Long id;
  private String name;
}
