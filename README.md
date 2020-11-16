#  wind

---
基于JdbcTemplate实现的快速开发框架,开箱即用,始终坚持低学习成本,所以它非常简单,使用它你会感觉很自然.

### 特性
 - 支持Mysql,Postgresql
 - 基于JdbcTemplate: 就是快,非常小巧,学习成本低
 - 拥抱Lambda: 全局Lambda/方法引用方式入参,完美支持参数类型推断
 - 特定查询: 查询指定列,返回基本类型(Long/BigDecimal/枚举等),过滤大字段
 - 枚举支持: Controller枚举参数,自定义枚举序列化
 - 类型支持: 支持自定义类型转换器,支持BitSet
 - 支持自动建表
 - 可定制: 
   - 自定义ID生成策略,默认使用雪花算法
   - 缓存: 支持缓存扩展,目前支持内存和redis缓存
   - 禁用公共字段

#### 迭代计划

 - [X] InterEnum枚举值由整型改为泛型. 
 - [X] controller枚举支持接收名称和value值,自动校验正确性
 - [X] 校验器工具类
 - [x] 全局异常处理
 - [x] 支持缓存扩展,目前支持内存和redis缓存
 - [x] 清除`redis`缓存改为scan指令
 - [ ] 支持关系映射
 - [ ] 前后端分离跨域配置,引入JWT

#### 测试项

![Mysql](Mysql-test.png?raw=true)

![Pgsql](Pgsql-test.png?raw=true)

### 开始使用

 1. 引入jar包:
    ```xml
    <dependency>
        <groupId>io.github.ramerf</groupId>
        <artifactId>wind-core</artifactId>
        <version>4.0.2-RELEASE</version>
    </dependency>
    ```
 2. 新建 pojo 实体`Foo`继承于`AbstractEntityPoJo`
    ```java
    @TableInfo
    public class Foo extends AbstractEntityPoJo {}
    ```
 3. 新建 service `FooService`继承于`BaseService`
    ```java
    public interface FooService extends BaseService<FooPoJo> {}
    ```
 4. 新建serviceImpl `FooServiceImpl`
    ```java
    @Service
    public class FooServiceImpl implements FooService {} 
    ```
 5. 注入`FooService`即可使用.

### 方法示例
#### 参考 wind-demo 模块 和 wind-core 测试代码 
#### service层
```java
@Slf4j
@Sql("classpath:db-mysql.sql")
@ExtendWith(SpringExtension.class)
@ActiveProfiles("mysql")
@SpringBootTest(classes = MysqlApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Mysql 测试")
public class BaseServiceTest {
  @Resource private FooService service;
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
            // .largeText("")
            .build();
  }

  @BeforeEach
  public void before() {
    foo.setId(id);
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:创建并返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet() {
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:创建并返回对象,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGetWithNull() {
    assertNotNull(service.createAndGetWithNull(foo, Collections.singletonList(Foo::getLargeText)));
  }

  @Test
  @DisplayName("单个更新:更新并返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet() {
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @DisplayName("单个更新:更新并返回对象,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGetWithNull() {
    assertNotNull(service.updateAndGetWithNull(foo, Collections.singletonList(Foo::getLargeText)));
  }

  @Test
  @DisplayName("count所有")
  @Transactional(rollbackFor = Exception.class)
  public void testCount1() {
    assertTrue(service.count() > 0);
  }

  @Test
  @DisplayName("count带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testCount2() {
    assertTrue(service.count(condition -> condition.gt(Foo::setId, 0L)) > 0);
  }

  @Test
  @DisplayName("count指定列带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testCount3() {
    final long count =
        service.count(query -> query.col(Foo::getId), condition -> condition.gt(Foo::setId, 0L));
    assertTrue(count > 0);
  }

  @Test
  @DisplayName("查询单个:通过id查询")
  @Transactional(rollbackFor = Exception.class)
  public void testGetById() {
    assertNotNull(service.getById(id));
  }

  @Test
  @DisplayName("查询单个:条件查询")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne1() {
    assertNotNull(service.getOne(condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne2() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne3() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:通过id列表查询")
  @Transactional(rollbackFor = Exception.class)
  public void testListByIds() {
    assertNotNull(service.listByIds(Arrays.asList(id, 2L, 3L)));
  }

  @Test
  @DisplayName("查询列表:条件查询")
  @Transactional(rollbackFor = Exception.class)
  public void testList1() {
    assertNotNull(service.list(condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @DisplayName("查询列表:指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testList2() {
    assertNotNull(
        service.list(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testList3() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @DisplayName("查询列表:条件查询指定页,带排序")
  @Transactional(rollbackFor = Exception.class)
  public void testList4() {
    assertNotNull(
        service.list(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testList5() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列指定页,返回任意对象,带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testList6() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:查询所有,指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testListAll1() {
    assertNotNull(service.listAll(query -> query.col(Foo::getId).col(Foo::getName)));
  }

  @Test
  @DisplayName("查询列表:查询所有,指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testListAll2() {
    assertNotNull(
        service.listAll(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件,带排序")
  @Transactional(rollbackFor = Exception.class)
  public void testPage1() {
    assertNotNull(
        service.page(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询分页:指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testPage2() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testPage3() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询分页:带条件指定列,返回任意对象,多个字段排序")
  @Transactional(rollbackFor = Exception.class)
  public void testPage4() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getId, SortColumn.Order.DESC).desc(Foo::getName).asc(Foo::getType),
            IdNameResponse.class));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate() {
    assertTrue(service.create(foo) > 0);
  }

  @Test
  @Order(2)
  @DisplayName("单个创建: 指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateWithNull() {
    assertTrue(service.createWithNull(foo, Collections.singletonList(Foo::getLargeText)) > 0);
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(service.createBatch(list).isPresent());
  }

  @Test
  @DisplayName("批量创建:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatchWithNull() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service.createBatchWithNull(list, Collections.singletonList(Foo::getName)).isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate() {
    assertEquals(service.update(foo), 1);
  }

  @Test
  @DisplayName("单个更新:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateWithNull() {
    assertEquals(service.updateWithNull(foo, Collections.singletonList(Foo::getLargeText)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateCondition() {
    assertEquals(service.update(condition -> condition.eq(Foo::setId, id), foo), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateConditionWithNull() {
    assertEquals(
        service.updateWithNull(
            condition -> condition.eq(Foo::setId, id),
            foo,
            Collections.singletonList(Foo::getName)),
        1);
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
    assertFalse(service.updateBatch(list).isPresent());
  }

  @Test
  @DisplayName("批量更新:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatchWithNull() {
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
    assertFalse(
        service.updateBatchWithNull(list, Collections.singletonList(Foo::getName)).isPresent());
  }

  @Test
  @Order(30)
  @DisplayName("单个删除:通过id删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete1() {
    assertEquals(service.delete(id), 1);
  }

  @Test
  @Order(31)
  @DisplayName("批量删除:条件删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete2() {
    assertEquals(service.delete(condition -> condition.eq(Foo::setId, id)), 1);
  }

  @Test
  @Order(32)
  @DisplayName("批量删除:通过id列表删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDeleteByIds() {
    assertTrue(service.deleteByIds(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
  }

  @Test
  @DisplayName("单个查询:默认不查询指定字段(大字段)")
  @Transactional(rollbackFor = Exception.class)
  public void testDontFetch() {
    // 默认不查询
    assertNull(service.getOne(condition -> condition.eq(Foo::setId, id)).getLargeText());
    // 指定查询该字段
    assertNotNull(
        service
            .getOne(
                query -> query.col(Foo::getLargeText), condition -> condition.eq(Foo::setId, id))
            .getLargeText());
  }
}

```
#### repository层(Query/Update)
支持任意表
```java
@Resource private PrototypeBean prototypeBean;

@GetMapping
@ApiOperation("使用Query")
public ResponseEntity<Rs<List<Long>> query() {
    // 获取查询列和查询条件对象
    final QueryColumn<Foo> queryColumn = QueryColumnFactory.getInstance(Foo.class);
    final Condition<Foo> condition = queryColumn.getCondition();
    // 指定查询列
    queryColumn.col(Foo::getId);
    // 指定查询条件
    condition.eq(Foo::setId, 1L);
    // 动态条件,第一个参数为false时,不会包含该条件
    condition.eq(false, Foo::setId, 2L);
    return Rs.ok(prototypeBean.query().select(queryColumn).where(condition).fetchAll(Long.class));
}

@GetMapping
@ApiOperation("使用Update")
public ResponseEntity<Rs<Integer>> update() {
    final Foo foo = Foo.builder().name("name").build();
    final int affectRow =
        prototypeBean
            .update()
            .from(Foo.class)
            .where((Consumer<ICondition<Foo>>) condition -> condition.eq(Foo::setId, 1L))
            .update(foo);
    return Rs.ok(affectRow);
}

```

#### 过滤大字段

```java
@TableColumn(dontFetch = true)
private String largeText;
```

#### 禁用公共字段

```yml
wind:
  # 禁用公共字段,可选:deleted,created_time,update_time
  disable-fields: create_time,update_time
```

#### 自动建表

```yml
wind:
  # 自动建表,扫描entity-package下包含@Entity/@TableInfo的类.可选值:none,create,update.默认:none
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

#### 自定义ID生成策略
默认使用雪花算法
```java
@Bean
public IdGenerator autoIncrementGenerator() {
    // 自定义id生成策略,下方为数据库自增写法
    return o -> null;
}
```

#### redis缓存
```yaml
wind:  
  redis-cache:
    # 默认开启
    enable: true
    # 前缀
    key-prefix: io.github.ramerf.wind
```
手动清除缓存
```java
@Resource private RedisCache redisCache;
redisCache.clear(Foo.class);
```
#### 可配置项
```yaml
wind:
  logic-delete-prop:
      # 是否启用逻辑删除,可以在类上使用@TableInfo(logicDelete = @LogicDelete(enable = true))属性覆盖
      enable: false
      # 逻辑删除字段名,@TableInfo会覆盖该配置
      column: deleted
      # 逻辑未删除值(默认为 false),@TableInfo会覆盖该配置
      not-delete: false
      # 逻辑已删除值(默认为 true),@TableInfo会覆盖该配置
      deleted: true
  # entity所在包路径,多个以,分割
  entity-package: io.github.ramerf.wind.demo.entity.pojo
  # 自动建表,扫描entity-package下包含@Entity/@TableInfo的类.可选值:none,create,update.默认:none
  ddl-auto: update
  # 禁用公共字段,可选:deleted,created_time,update_time
  disable-fields: create_time,update_time
  # 批量操作时每次处理的大小,默认为150
  batch-size: 500
  # 是否自定义枚举反序列化,默认为false.设置为true时,可能需要编写枚举反序列化代码
  custom-enum-deserializer: false
  cache:
    # 缓存类型.可选值: redis,memory 默认memory
    type: redis
    # 缓存key前缀
    key-prefix: io.github.ramerf.wind
  # 用于分布式id雪花算法
  snowflake-prop:
    worker-id: 3
    data-center-id: 2
  # 启用默认mvc配置,默认true启用,如果默认配置与项目需求不兼容时可设置为false禁用
  enable-web-mvc-configurer: false
  # 新增/更新时写入值为null的属性,默认写入
  write-null-prop: false
```

#### 可用注解

- @TableInfo 用于指定表信息,配合@Entity使用
- @TableColumn 用于指定列信息
- @LogicDelete 用于@TableInfo注解指定逻辑删除信息
- @TypeHandler 当全局类型处理器不满足需求时,用于指定特定字段使用的类型处理器
- @CreateTimestamp 指定该字段值为创建时间
- @UpdateTimestamp 指定该字段为更新时间

#### 通用命名风格

代码格式化使用: google-java-format

- 单个: getXxx/getOne
- 多个: listXxx
- 分页: pageXxx
- 批量: xxxBatch
- 获取/构造实例: of, fromXxx, getInstance, by

#### Issue & Pull request

欢迎提Issue 和 Pull request

#### 联系我

如果你在使用本项目时遇到问题,可以通过以下方式联系到我,我将提供免费技术支持

- WX: ramer-
- QQ: 1390635973
- Mail: [1390635973@qq.com](mailto:1390635973@qq.com)

#### 开源协议

[GNU GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses)