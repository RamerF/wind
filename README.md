#  wind

---

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.ramerf/wind-core/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.ramerf/wind-core)

基于JdbcTemplate实现的快速开发框架，简单高效。

### 特性

 - 支持Mysql,Postgresql
 - 基于JdbcTemplate：就是快，非常小巧
 - 拥抱Lambda：全局Lambda/方法引用方式入参，完美支持参数类型推断
 - 特定查询：查询指定列，支持返回基本类型(Long/BigDecimal/枚举等)
 - 类型支持：支持自定义类型转换器，支持BitSet
 - 支持自动建表
 - service支持任意表互操作
 - 域实体（Domain）支持写入操作
 - 可配置是否写入null值
 - 支持关系映射

#### 迭代计划

 - [ ] 支持指定表的索引@TableIndexes(@TableIndex(name= "", fields = {""}, unique = false ))

### 快速开始

 1. 引入jar包:
    ```xml
    <dependency>
        <groupId>io.github.ramerf</groupId>
        <artifactId>wind-core</artifactId>
        <version>4.0.6</version>
    </dependency>
    ```
    
 2. 新建实体`Foo`
    ```java
    @TableInfo
    public class Foo {
      // 实体必须有主键
      @javax.persistence.Id private Long id;
    }
    ```
    
 3. OK，可以使用了！
    
    ```java
    GenericService service = GenericService.with(Foo.class, Long.class);
    // service可以操作任意实体 service.create(bar)
    service.getById(1L);
    ```

### 当然，通常你还会定义service

 1. 新建 service `FooService`继承于`BaseService`

    ```java
    public interface FooService extends BaseService<Foo, Long> {}
    ```

 2. 新建serviceImpl `FooServiceImpl`
    ```java
    @Service
    public class FooServiceImpl implements FooService {}
    ```

 3. 注入`FooService`即可使用.

### 方法示例
参考 `wind-demo` 模块 和 `wind-core` 测试代码 

```java
@Slf4j
@Sql("classpath:db-mysql.sql")
@ExtendWith(SpringExtension.class)
@ActiveProfiles("mysql")
@SpringBootTest(classes = MysqlApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Mysql 测试")
public class BaseServiceTest {
  private GenericService<Foo, Long> service;
  private static final Foo foo;
  private static final Long id = 10000L;

  static {
    foo =
        Foo.builder()
            .id(1L)
            .name("test")
            .textString("textString")
            .bigDecimal(BigDecimal.valueOf(100))
            .type(Type.SPORT)
            .column("non_match_column")
            .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
            // .bigText("")
            .isNumber(false)
            .isNull(false)
            .string(true)
            .nonNull(true)
            .typeJson(Type.SPORT)
            .typesJson(Arrays.asList(Type.PHONE, Type.SPORT))
            .createTime(LocalDateTime.now())
            .build();
  }

  @BeforeEach
  public void before() {
    foo.setId(id);
    service = GenericService.with(Foo.class, Long.class);
  }

  @Test
  @DisplayName("条件构造工具类")
  public void testCnds() {
    final Cnds<Foo> cnds =
        // 指定操作的表
        Cnds.of(Foo.class)
            // 条件
            .gt(Foo::setId, 0L)
            // 自定义sql
            .and("name is not null")
            // 分页
            .limit(1, 10)
            // 分组
            .groupBy(Foo::getName)
            // 排序
            .orderBy(Foo::getId, Direction.DESC);
    log.info("testCnds:[{}]", cnds.getString());
    final QueryColumn<Foo> queryColumn =
        QueryColumn.of(Foo.class)
            // 查询指定列
            .col(Foo::getId)
            .col(Foo::getName)
            // 自定义sql
            .col(
                "(case name when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) as name,id");
    log.info("testCnds:[{}]", queryColumn.getString());
  }

  @Test
  @DisplayName("统计")
  public void testCount() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L);
    assertTrue(service.count(cnds) > 0);
  }

  @Test
  @DisplayName("查询单个")
  public void testGetOne() {
    // 通过id查询
    assertNotNull(service.getById(id));
    // 条件查询
    assertNotNull(service.getOne(Cnds.of(Foo.class).eq(Foo::setId, id)));
    // 条件查询指定列
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    final QueryColumn<Foo> queryColumn =
        QueryColumn.of(Foo.class).col(Foo::getName).col(Foo::getId);
    assertNotNull(service.getOne(cnds, queryColumn));
    // 条件查询排序
    assertNotNull(service.getOne(Cnds.of(Foo.class).limit(1).orderBy(Foo::getId, Direction.DESC)));
    // 返回任意对象
    assertNotNull(
        service.getOne(
            Cnds.of(Foo.class).eq(Foo::setId, id),
            QueryColumn.of(Foo.class).col(Foo::getId),
            IdNameResponse.class));
    // 自定义sql
    service.getOne(
        Cnds.of(Foo.class).eq(Foo::setId, id).and("name is not null"),
        QueryColumn.of(Foo.class)
            .col(
                "(case name when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) as name,id"));
    // 自定义sql
    assertNotNull(service.fetchOneBySql("select id,name from foo limit 1", IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表")
  public void testList() {
    // 通过id列表查询
    assertNotNull(service.listByIds(Arrays.asList(id, 2L, 3L)));
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    // 条件查询
    assertNotNull(service.list(cnds));
    // 查询指定列
    final QueryColumn<Foo> queryColumn =
        QueryColumn.of(Foo.class).col(Foo::getName).col(Foo::getId);
    assertNotNull(service.list(cnds, queryColumn, IdNameResponse.class));
    // 查询指定页
    cnds.limit(1, 10).orderBy(Foo::getId, Direction.DESC);
    assertNotNull(service.list(cnds));
    // 指定返回对象
    assertNotNull(service.list(cnds, IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页")
  public void testPage() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L).limit(1, 10).orderBy(Foo::getName);
    assertNotNull(service.page(cnds));
    // 指定列
    assertNotNull(service.page(cnds, QueryColumn.of(Foo.class).col(Foo::getId).col(Foo::getName)));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate() {
    foo.setId(null);
    assertTrue(service.create(foo) > 0);
    // 保存指定字段
    final Fields<Foo> fields =
        Fields.of(Foo.class).include(Foo::getAge, Foo::isString, Foo::isNumber);
    // 排除指定字段
    // fields.exclude(Foo::getAge)
    assertTrue(service.create(foo, fields) > 0);
    // 返回当前对象相当于 create + getOne
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch() {
    foo.setId(null);
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service
            .createBatch(
                list,
                Fields.of(Foo.class)
                    .include(
                        Foo::getName,
                        Foo::getTextString,
                        Foo::getBigText,
                        Foo::getType,
                        Foo::getColumn))
            .isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate() {
    assertEquals(service.update(foo), 1);
    foo.setName("<" + LocalDateTime.now() + ">");
    // 指定属性
    assertEquals(service.update(foo, Fields.of(Foo.class).include(Foo::getName)), 1);
    // 条件更新
    assertEquals(service.update(foo, Cnds.of(Foo.class).eq(Foo::setId, id)), 1);
    // 条件更新指定字段
    assertEquals(
        service.update(
            foo, //
            Fields.of(Foo.class).include(Foo::getName),
            Cnds.of(Foo.class).eq(Foo::setId, id)),
        1);
    // 返回当前对象相当于 update + getOne
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @DisplayName("批量更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("test" + i * i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    // 可选指定更新字段
    assertFalse(service.updateBatch(list, Fields.of(Foo.class).include(Foo::getName)).isPresent());
  }

  @Test
  @Order(20)
  @DisplayName("删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete() {
    // 通过id删除
    assertEquals(service.delete(id), 1);
    // 通过id列表删除
    assertTrue(service.deleteByIds(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
    // 条件删除
    assertEquals(service.delete(Cnds.of(Foo.class).eq(Foo::setId, id)), 1);
  }

  @Test
  @Order(21)
  @DisplayName("域对象Domain")
  @Transactional(rollbackFor = Exception.class)
  public void testDomain() {
    // 需要对象继承Domain: public class Foo extends Domain<Foo, Long>
    foo.setId(null);
    assertTrue(foo.create() > 0);
    foo.setId(id);
    assertTrue(foo.update(Fields.of(Foo.class).include(Foo::getName)) > 0);
    assertTrue(foo.delete(Cnds.of(Foo.class).eq(Foo::setId, id)) > 0);
  }

  /**
   * @author ramer
   * @since 2020/8/5
   */
  @Getter
  @Setter
  public static class IdNameResponse {
    private Long id;
    private String name;
  }
}
```

#### 自定义类型处理器

```java
@Bean
public ITypeHandler customTypeHandler() {}
```

#### 自动建表

```yml
wind:
  # 自动建表,扫描entity-package下包含@TableInfo的类.可选值:none,create,update.默认:none
  ddl-auto: update
```

#### 自定义枚举序列化

 - 默认枚举序列化为 key:value 格式
    ```json
    {
        "value": "值",
        "desc": "中文描述"
    }
    ```
 - 自定义
    ```java
    /** 自定义枚举的序列化格式,只返回value. */
    @Bean
    public InterEnumSerializer interEnumSerializer() {
        return InterEnum::value;
    }
    ```

#### 自定义主键生成策略

默认使用雪花算法
```java
@Bean
public IdGenerator autoIncrementGenerator() {
    // 自定义id生成策略,下方为数据库自增写法
    return o -> null;
}
```

#### 缓存

```yaml
wind:  
  cache:
    # 缓存类型.可选值: redis,memory,none 默认none禁用缓存
    type: redis
    # 缓存key前缀
    key-prefix: io.github.ramerf.wind
```
##### 手动清除缓存

```java
@Resource private Cache cache;
cache.clear(Foo.class);
```
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

#### 通用命名风格

代码格式化使用: google-java-format

- 单个: getXxx/getOne
- 多个: listXxx
- 分页: pageXxx
- 批量: xxxBatch
- 获取/构造实例: of，getInstance，fromXxx，by，with

#### Issue & Pull request

欢迎提Issue 和 Pull request

#### 联系我

如果你在使用本项目时遇到问题,可以通过以下方式解决

- QQ群: 975225058
- Mail: [1390635973@qq.com](mailto:1390635973@qq.com)

#### 开源协议

[GNU GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses)