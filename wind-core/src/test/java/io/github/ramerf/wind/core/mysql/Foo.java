package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.BitSetBlobTypeHandler;
import io.github.ramerf.wind.core.service.UpdateService.Fields;
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
public class Foo extends AbstractEntityPoJo {
  private String name;

  /** 基本类型. */
  private int age;

  @TableColumn(columnDefinition = "text")
  private String textString;

  @TypeHandler(BitSetBlobTypeHandler.class)
  @TableColumn(columnDefinition = "blob")
  private BitSet bitSet;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 smallint/int */
  @TableColumn(columnDefinition = "smallint")
  private Type type;

  /** 继承{@link InterEnum}的枚举类型 可对应数据库类型 varchar */
  @TableColumn(columnDefinition = "varchar(1)")
  private Alphabet alphabet;

  @TableColumn(columnDefinition = "numeric(5,2)")
  private BigDecimal bigDecimal;

  /** 字段与数据库列不对应时,使用{@link TableColumn#name()}指定数据库字段名. */
  @TableColumn(name = "non_match_column")
  private String column;

  /** 自定义逻辑删除字段. */
  @TableColumn(defaultValue = "false")
  private boolean hasDeleted;

  /** 大文本字段,测试{@link Fields} include/exclude. */
  @TableColumn(comment = "大文本字段,测试Fields include/exclude", columnDefinition = "text")
  private String largeText;

  /* boolean类型. */
  private boolean isNumber;
  private boolean string;
  private Boolean isNull;
  private Boolean nonNull;

  public enum Type implements InterEnum<Integer> {
    /** 类别 */
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

  public enum Alphabet implements InterEnum<String> {
    /** Alphabet. */
    _A("A", "A"),
    _B("B", "B");

    private final String value;
    private final String desc;

    Alphabet(String value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public String value() {
      return this.value;
    }

    @Override
    public String desc() {
      return this.desc;
    }
  }
}
