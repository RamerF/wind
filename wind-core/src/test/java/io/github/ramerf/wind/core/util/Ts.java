package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.TableColumn;

/** 测试类. */
public class Ts {
  @TableColumn(updatable = false)
  private long id;

  @TableColumn(name = "alia")
  private String name;

  @TableColumn(insertable = false)
  private String db;

  private transient String idName;
}
