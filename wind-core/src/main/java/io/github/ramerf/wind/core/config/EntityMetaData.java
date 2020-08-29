package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.support.EntityInfo;
import java.util.List;
import lombok.Data;

/**
 * 自动扫描的所有实体信息.
 *
 * @author ramer
 * @since 13/08/2020
 */
@Data
public class EntityMetaData {
  private List<EntityInfo> entityInfos;
}
