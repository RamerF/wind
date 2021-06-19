package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.LambdaDomain;
import io.github.ramerf.wind.core.handler.TypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.*;
import io.github.ramerf.wind.core.service.InterService.Fields;
import java.math.BigDecimal;
import java.util.*;
import javax.persistence.Id;
import lombok.*;

/**
 * @author ramer
 * @since 2019/12/16
 */
@TableInfo(
    name = "foo",
    comment = "测试表",
    logicDelete = @LogicDelete(enable = true, fieldName = "hasDeleted"))
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Foo extends LambdaDomain<Foo, Long> {
  @Id private Long id;
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
  private String bigText;

  /* boolean类型. */
  private boolean isNumber;
  private boolean string;
  private Boolean isNull;
  private Boolean nonNull;

  @TableColumn(columnDefinition = "text", comment = "对象集合转存json,可指定集合类型")
  @TypeHandler(ObjectCollectionToJsonTypeHandler.class)
  private List<Type> typesJson = new LinkedList<>();

  @TableColumn(columnDefinition = "text", comment = "对象转存json")
  @TypeHandler(ObjectToJsonTypeHandler.class)
  private Type typeJson;

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
