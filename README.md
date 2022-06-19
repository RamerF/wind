#  wind

---

[TOC]

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.ramerf/wind-core/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.ramerf/wind-core)

一款小巧的持久化框架，旨在简化数据库操作，支持自动创建表和索引。

### 支持数据库

 - Mysql
 - PostgreSQL

### 迭代计划

 - [ ] SQLite 支持

### 快速开始

 1. 引入jar包:
    ```xml
    <dependency>
        <groupId>io.github.ramerf</groupId>
        <artifactId>wind-core</artifactId>
        <version>5.0.0</version>
    </dependency>
    ```
    
 2. 新建实体`Foo`
    ```java
    @TableInfo // 该注解是可选的
    public class Foo {
      // 实体必须有主键
      @javax.persistence.Id private Long id;
    }
    ```
    
 3. OK，可以使用了！
    
    ```java
    GenericService service = GenericService.with(Foo.class, Long.class);
    // service可以操作任意实体 service.create(bar)
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L).limit(1, 10).orderBy(Foo::getName);
    service.page(cnds);
    ```

### 定义service

 需要继承于`BaseService`

```java
public interface FooService extends BaseService<Foo, Long> {}
```

#### 自定义类型处理器

#### 自动建表

#### 可配置项

```yaml
wind:
  logic-delete-prop:
      # 是否启用逻辑删除,可以在类上使用@TableInfo(logicDelete = @LogicDelete(enable = true))属性覆盖
      enable: false
      # 逻辑删除字段名
      field-name: deleted
      # 逻辑未删除值(默认为 false)
      not-delete: false
      # 逻辑已删除值(默认为 true)
      deleted: true
  # entity所在包路径,多个以,分割;如果不需要自动建表可以不配置该项
  entity-package: io.github.ramerf.wind.demo.entity.pojo
  # 自动建表,扫描entity-package下包含@TableInfo的类.可选值:none,create,update.默认:none
  ddl-auto: update
  # 批量操作时每次处理的大小,默认为150
  batch-size: 500
  # 是否自定义枚举反序列化,默认为false.设置为true时,可能需要编写枚举反序列化代码
  custom-enum-deserializer: false
  cache:
    # 缓存类型.可选值: redis,memory,none 默认none禁用缓存
    type: redis
    # 缓存key前缀
    key-prefix: io.github.ramerf.wind
  # 用于雪花算法
  snowflake-prop:
    worker-id: 3
    data-center-id: 2
  # 新增/更新时写入值为null的属性,默认写入
  write-null-prop: false
```

#### 可用注解

- @TableInfo 用于指定表信息
- @TableColumn 用于指定列信息
- @LogicDelete 用于@TableInfo注解指定逻辑删除信息
- @TypeHandler 当全局类型处理器不满足需求时,用于指定特定字段使用的类型处理器
- @CreateTimestamp 指定该字段值为创建时间
- @UpdateTimestamp 指定该字段为更新时间
- @ManyToOne 多对一
- @OneToMany 一对多
- @OneToOne 一对一

#### 联系我

如果您在使用本项目时遇到问题,请邮件联系

- Mail: [1390635973@qq.com](mailto:1390635973@qq.com)

#### 开源协议

[GNU GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses)