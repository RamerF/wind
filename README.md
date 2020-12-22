#  wind

---
基于JdbcTemplate实现的快速开发框架,学习成本超低.

### 特性
 - 支持Mysql,Postgresql
 - 基于JdbcTemplate：就是快,非常小巧,学习成本超低
 - 支持任意类型主键
 - 拥抱Lambda：全局Lambda/方法引用方式入参,完美支持参数类型推断
 - 特定查询：查询指定列,支持返回基本类型(Long/BigDecimal/枚举等)
 - 特定写入：指定写入列
 - 枚举支持：Controller枚举参数,自定义枚举序列化
 - 类型支持：支持自定义类型转换器,支持BitSet
 - 支持自动建表
 - 可定制：
   - 自定义ID生成策略,默认使用雪花算法
   - 枚举序列化
   - 缓存：支持缓存扩展，目前支持内存和redis缓存
   - 是否写入null值

#### 迭代计划

 - [ ] 支持关系映射
 - [ ] 支持指定表的索引@TableIndexes(@TableIndex(name= "", fields = {""}, unique = false ))

#### 测试项

![Mysql](Mysql-test.png?raw=true)

![Pgsql](Pgsql-test.png?raw=true)

### 快速开始

 1. 引入jar包:
    ```xml
    <dependency>
        <groupId>io.github.ramerf</groupId>
        <artifactId>wind-core</artifactId>
        <version>4.0.3</version>
    </dependency>
    ```
    
 2. 新建 pojo 实体`Foo`继承于`AbstractEntityPoJo`
    ```java
    @TableInfo
    public class Foo extends AbstractEntityPoJo<Foo, Long> {
      @Id private Long id;
    }
    ```

 3. OK，可以使用了！
    
    ```java
    GenericService service = GenericService.with(Foo.class, Long.class);
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
            .build();
  }

  @BeforeEach
  public void before() {
    foo.setId(id);
    service = GenericService.with(Foo.class, Long.class);
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
  @DisplayName("查询单个:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne4() {
    service.getOne(
        query ->
            query.col(
                "(case name when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) as name,id"),
        condition -> condition.eq(Foo::setId, 10000L).and("name is not null"));
  }

  @Test
  @DisplayName("查询单个:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne5() {
    assertNotNull(service.getOne("select id,name from foo limit 1", IdNameResponse.class));
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
    assertNotNull(service.list(condition -> condition.gt(Foo::setId, 0L), 1, 10));
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
  @DisplayName("查询列表:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public Rs<List<Foo>> testListAll3() {
    return Rs.ok(service.listAll("select * from foo", Foo.class));
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
  public void testCreate1() {
    foo.setId(null);
    assertNotNull(service.create(foo));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建: 指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate2() {
    foo.setId(null);
    // assertTrue(service.create(foo, fields -> fields.exclude(Foo::getAge)) > 0);
    assertNotNull(
        service.create(foo, fields -> fields.include(Foo::getAge, Foo::isString, Foo::isNumber)));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:域对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate3() {
    foo.setId(null);
    final Consumer<Fields<Foo>> consumer =
        fields ->
            fields
                .include(Foo::getName, Foo::getAge, Foo::isString, Foo::isNumber)
                .exclude(Foo::getBigText);
    assertNotNull(foo.create(consumer));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet1() {
    foo.setId(null);
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:返回对象,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet2() {
    foo.setId(null);
    assertNotNull(service.createAndGet(foo, fields -> fields.exclude(Foo::getBigText)));
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch1() {
    foo.setId(null);
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
  @DisplayName("批量创建:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch2() {
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
        service
            .createBatch(
                list,
                fields -> fields.include(Foo::getName, Foo::getAge, Foo::isString, Foo::isNumber))
            .isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate1() {
    assertEquals(service.update(foo), 1);
  }

  @Test
  @DisplayName("单个更新:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate2() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertEquals(service.update(foo, fields -> fields.include(Foo::getName)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate3() {
    assertEquals(service.updateByCondition(foo, condition -> condition.eq(Foo::setId, id)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate4() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertEquals(
        service.update(
            foo, //
            fields -> fields.include(Foo::getName),
            condition -> condition.eq(Foo::setId, id)),
        1);
  }

  @Test
  @DisplayName("单个更新:返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet1() {
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @DisplayName("单个更新:返回对象,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet2() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertNotNull(service.updateAndGet(foo, fields -> fields.include(Foo::getName)));
  }

  @Test
  @DisplayName("批量更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch1() {
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
  @DisplayName("批量更新:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch2() {
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
    assertFalse(service.updateBatch(list, fields -> fields.include(Foo::getName)).isPresent());
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

  /**
   * @author Tang Xiaofeng
   * @since 2020/8/5
   */
  @Getter
  @Setter
  public static class IdNameResponse implements AbstractEntity {
    private Long id;
    private String name;
  }
}

```


#### repository层(Query/Update)

```java
@Slf4j
@RestController
@RequestMapping("/query-update")
@Api(tags = "Query/Update使用示例")
public class QueryUpdateController {
  @Resource private PrototypeBean prototypeBean;

  @GetMapping("/create")
  @ApiOperation("使用Update,创建")
  public Rs<Product> create() {
    final Product product =
        Product.builder()
            .id(LocalDateTime.now().toString())
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .type(Type.REALITY)
            .date(new Date())
            .localDate(LocalDate.now())
            .createTime(LocalDateTime.now())
            .build();
    final Update<Product> update = prototypeBean.update(Product.class);
    update.create(product);
    return Rs.ok(product);
  }

  @GetMapping("/update")
  @ApiOperation("使用Update,条件更新,指定更新字段")
  public Rs<Integer> update() {
    final Product product =
        Product.builder()
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .build();
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    // 指定更新字段:title,name
    final Fields<Product> fields =
        Fields.with(Product.class).include(Product::getTitle, Product::getName);
    // 获取Update实例
    final Update<Product> update = prototypeBean.update(Product.class);
    final int affectRow =
        update.where(condition.eq(Product::setId, "string-id")).update(product, fields);
    return Rs.ok(affectRow);
  }

  @GetMapping(value = "/query", params = "type=1")
  @ApiOperation("使用Query,条件组,or条件拼接")
  public Rs<List<Product>> query1() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final CustomCondition<Product> condition = CustomCondition.getInstance(queryColumn);
    // 条件组 (id='string-id' or name ='name')
    LambdaConditionGroup<Product> conditionGroup = LambdaConditionGroup.getInstance(queryColumn);
    conditionGroup.orEq(Product::setId, "string-id");
    conditionGroup.orEq(Product::setName, "name");
    // 获取Update实例
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.notEq(Product::setId, "string-id").and(conditionGroup))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=2")
  @ApiOperation("使用Query,or拼接")
  public Rs<List<Product>> query2() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    // 查询条件: (id='ramer' and name like 'a%') or (id='jerry' and name like 'b%')
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    condition
        .and(group -> group.andEq(Product::setId, "ramer").andLike(Product::setName, "a%"))
        .or(group -> group.andEq(Product::setId, "jerry").andLike(Product::setName, "b%"));
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query.select(queryColumn).where(condition).fetchAll(Product.class);
    log.info("query2:[{}]", products);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=3")
  @ApiOperation("使用Query,指定字段")
  public Rs<List<IdNameResponse>> query3() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<IdNameResponse> products =
        query
            .select(queryColumn.col(Product::getId).col(Product::getName))
            .where(condition.like(Product::setName, "name%"))
            .fetchAll(IdNameResponse.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=4")
  @ApiOperation("使用Query,返回基本类型")
  public Rs<List<Long>> query4() {
    final QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    final LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final Query<Foo> query = prototypeBean.query(Foo.class);
    final List<Long> ids =
        query
            .select(queryColumn.col(Foo::getId))
            .where(condition.eq(Foo::setName, "name"))
            .fetchAll(Long.class);
    return Rs.ok(ids);
  }

  @GetMapping(value = "/query", params = "type=5")
  @ApiOperation("使用Query,使用StringCondition构造条件")
  public Rs<List<Product>> query5() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final StringCondition<Product> condition = StringCondition.getInstance(queryColumn);
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.eq("id", "string-id").lt("create_time", LocalDateTime.now()))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=6")
  @ApiOperation("使用Query,自定义sql查询)")
  public Rs<List<Product>> query6() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    // 执行连表: foo.id=account.id
    condition.eq(Product::getId, queryColumn, Product::getId);
    final Query<Product> query = Query.getInstance(Product.class);
    queryColumn.col(
        "(case title when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) title,id");
    return Rs.ok(query.select(queryColumn).where(condition.and("id<>'1'")).fetchAll(Product.class));
  }

  @GetMapping(value = "/query", params = "type=7")
  @ApiOperation("使用Query,自定义sql查询")
  public Rs<List<Product>> query7() {
    final Query<Product> query = Query.getInstance(Product.class);
    // 单条记录
    final Product product =
        query.fetchOneBySql("select * from product where title = 'halo'", Product.class);
    // 多条记录
    final List<Product> products =
        query.fetchAllBySql("select * from product where id='string-id'", Product.class);
    return Rs.ok(products);
  }

  /** 不建议使用该操作,如果是复杂的sql,建议直接使用sql查询 {@link #query6()},{@link #query7()} */
  @GetMapping(value = "/query", params = "type=8")
  @ApiOperation("使用Query,groupBy,sum")
  public Rs<List<GroupBySum>> query8() {
    QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final QueryEntityMetaData<Foo> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    final GroupByClause<Foo> clause = queryEntityMetaData.getGroupByClause();
    final Query<Foo> query = prototypeBean.query(Foo.class);
    final List<GroupBySum> list =
        query
            .select(queryColumn.sum(Foo::getId, "big_decimal").col(Foo::getName, "name"))
            .where(condition)
            .groupBy(clause.col(Foo::getName))
            .fetchAll(GroupBySum.class);
    return Rs.ok(list);
  }

  @GetMapping(value = "/extend-condition")
  @ApiOperation("使用Query,返回任意对象)")
  public Rs<List<Product>> extendCondition() {
    // 可指定查询列
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    // 扩展Condition
    final CustomCondition<Product> condition = CustomCondition.getInstance(queryColumn);
    // 获取Query实例
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.notEq(Product::setId, "string-id"))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  /** 示例:扩展条件{@link Condition}. */
  public static class CustomCondition<T extends AbstractEntityPoJo<T, ?>>
      extends AbstractCondition<T> {
    public CustomCondition(final QueryColumn<T> queryColumn) {
      super(queryColumn);
    }

    public CustomCondition(final Class<T> clazz, final String tableName, final String tableAlia) {
      super(clazz, tableName, tableAlia);
    }

    public static <T extends AbstractEntityPoJo<T, ?>> CustomCondition<T> getInstance(
        final QueryColumn<T> queryColumn) {
      return new CustomCondition<>(queryColumn);
    }

    public <V> CustomCondition<T> notEq(@Nonnull final IConsumer<T, V> field, final V value) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator())
              .concat(field.getColumn())
              .concat("<>")
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
      return this;
    }

    public CustomCondition<T> and(@Nonnull LambdaConditionGroup<T> group) {
      if (group.getCondition().getValueTypes().size() > 0) {
        conditionSql.add(
            (conditionSql.size() > 0 ? AND.operator() : "")
                .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
        valueTypes.addAll(group.getCondition().getValueTypes());
      }
      return this;
    }
  }

  @Data
  public static class GroupBySum {
    private BigDecimal bigDecimal;
    private String name;
  }
}

```

#### 自定义类型处理器

```java
@Bean
public ITypeHandler customTypeHandler() {
```

}
```

#### 自动建表

​```yml
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

#### 自定义ID生成策略
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
  # entity所在包路径,多个以,分割
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

- @TableInfo 用于指定表信息
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
- 获取/构造实例: of, getInstance, fromXxx, by, with

#### Issue & Pull request

欢迎提Issue 和 Pull request

#### 联系我

如果你在使用本项目时遇到问题,可以通过以下方式解决

- QQ群: 975225058
- Mail: [1390635973@qq.com](mailto:1390635973@qq.com)

#### 开源协议

[GNU GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses)