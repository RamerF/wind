package io.github.ramerf.wind.core.mysql.entity.pojo;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.BitSetBlobTypeHandler;
import java.math.BigDecimal;
import java.util.BitSet;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/16
 */
@TableInfo(name = "foo", comment = "测试表", logicDelete = @LogicDelete(column = "has_deleted"))
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Foo extends AbstractEntityPoJo<Foo> {
  private String name;

  /** 基本类型. */
  private int age;

  @TableColumn(columnDefinition = "text")
  private String textString;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @TableColumn(columnDefinition = "smallint")
  private Type type;

  @TableColumn(columnDefinition = "numeric(5,2)")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link TableColumn#name()}指定数据库字段名. */
  @TableColumn(name = "non_match_column")
  private String column;

  /** 自定义逻辑删除字段. */
  @TableColumn(defaultValue = "false")
  private boolean hasDeleted;

  @TypeHandler(BitSetBlobTypeHandler.class)
  @TableColumn(columnDefinition = "blob")
  private BitSet bitSet;

  /** 大文本字段,测试默认不拉取该字段. */
  @TableColumn(dontFetch = true, comment = "大文本字段,测试默认不拉取该字段", columnDefinition = "text")
  private String largeText;

  public enum Type implements InterEnum<Integer> {
    /** 商品类别 */
    PHONE(0, "手机"),
    SPORT(1, "运动");

    private final int value;
    private final String desc;

    Type(int value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public Integer value() {
      return this.value;
    }

    @Override
    public String desc() {
      return this.desc;
    }
  }
}
