package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

/**
 * The type Wind configuration.
 *
 * @since 2020 /1/14
 * @author Tang Xiaofeng
 */
@Data
@Slf4j
@ConfigurationProperties("wind")
public class WindConfiguration {

  /** 逻辑删除配置. */
  @NestedConfigurationProperty private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String entityPackage = "";

  /**
   * 枚举所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String enumPackage = "";

  /** 是否自定义枚举反序列化.设置为true时,可能需要编写枚举反序列化代码. */
  private boolean customEnumDeserializer = false;

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 150;

  /** 是否启用通用mvc配置. */
  private boolean enableWebMvcConfigurer = true;

  /** 禁用{@link AbstractEntityPoJo}中的公共字段. */
  private List<CommonField> disableFields = new ArrayList<>();

  /** 自动更新表模式. */
  private DdlAuto ddlAuto;

  /** 数据库方言全路径. */
  private String dialect;

  /** 雪花分布式id. */
  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();

  /** Redis分布式缓存配置. */
  @NestedConfigurationProperty private RedisCache redisCache = new RedisCache();

  public void setLogicDeleteProp(final LogicDeleteProp logicDeleteProp) {
    this.logicDeleteProp = logicDeleteProp;
    // 如果逻辑删除字段已被禁用,给个提示
    final String logicDeleteColumn = logicDeleteProp.getColumn();
    if (disableFields.size() > 0
        && logicDeleteColumn.equals(AbstractEntityPoJo.LOGIC_DELETE_COLUMN_NAME)
        && getDisableFields().stream()
            .anyMatch(field -> field.getColumn().equals(logicDeleteColumn))) {
      log.warn("逻辑删除字段[{}]已被禁用,将启用全局物理删除!", logicDeleteColumn);
      logicDeleteProp.setEnable(false);
    }
  }

  public void setDisableFields(final List<CommonField> disableFields) {
    this.disableFields = disableFields;
    // 如果逻辑删除字段已被禁用,给个提示
    final String logicDeleteColumn = logicDeleteProp.getColumn();
    if (logicDeleteColumn.equals(AbstractEntityPoJo.LOGIC_DELETE_COLUMN_NAME)
        && disableFields.stream().anyMatch(field -> field.getColumn().equals(logicDeleteColumn))) {
      log.warn("逻辑删除字段[{}]已被禁用,将启用全局物理删除!", logicDeleteColumn);
      logicDeleteProp.setEnable(false);
    }
  }

  /**
   * Redis 缓存配置.
   *
   * @since 2020.08.23
   * @author Tang Xiaofeng
   */
  @Setter
  @Getter
  public static class RedisCache {
    /** 是否启用. */
    private boolean enable = true;

    /** 缓存key前缀. */
    private String keyPrefix = "io.github.ramerf.wind";
  }

  /**
   * The enum Ddl auto.
   *
   * @author Tang Xiaofeng
   */
  public enum DdlAuto {
    /** Create ddl auto. */
    CREATE,
    /** Update ddl auto. */
    UPDATE,
    /** None ddl auto. */
    NONE
  }

  /**
   * PoJo公共字段.
   *
   * @author Tang Xiaofeng
   */
  @Slf4j
  @SuppressWarnings("JavadocReference")
  public enum CommonField {
    /** {@link AbstractEntityPoJo#deleted}. */
    DELETED {
      @Override
      public Field getField() {
        return EntityInfo.DEFAULT_LOGIC_DELETE_FIELD;
      }

      @Override
      public String getColumn() {
        return AbstractEntityPoJo.LOGIC_DELETE_COLUMN_NAME;
      }
    },
    /** {@link AbstractEntityPoJo#createTime}. */
    CREATE_TIME {
      @Override
      public Field getField() {
        return EntityInfo.DEFAULT_CREATE_TIME_FIELD;
      }

      @Override
      public String getColumn() {
        return AbstractEntityPoJo.CREATE_TIME_COLUMN_NAME;
      }
    },
    /** {@link AbstractEntityPoJo#updateTime}. */
    UPDATE_TIME {
      @Override
      public Field getField() {
        return EntityInfo.DEFAULT_UPDATE_TIME_FIELD;
      }

      @Override
      public String getColumn() {
        return AbstractEntityPoJo.UPDATE_TIME_COLUMN_NAME;
      }
    };

    public abstract Field getField();

    public abstract String getColumn();
  }
}
