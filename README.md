# wind

---
基于spring-boot的快速开发框架,可与Mybatis/Hibernate共存.

### 特性
 - 查询支持返回指定列,返回基本类型(Long/BigDecimal等)
 - lambda方式构造条件,支持类型推断
 - 基于jdbc-template
 - service层切入,repository层依然可以使用其它持久化框架
 - 自定义枚举序列化
 - 自定义ID生成策略
 - 默认开启频繁请求拦截
 - 默认开启redis分布式缓存
 - controller(ControllerHelper),service,repository(Query/Update)三层无耦合,每层都可独立使用
 
### 开始使用
 1. 引入jar包:
    ```xml
    <dependency>
        <groupId>io.github.ramerf</groupId>
        <artifactId>wind-core</artifactId>
        <version>3.7.0-RELEASE</version>
    </dependency>
    ```
 2. 新建 pojo 实体`Foo`继承于`AbstractEntityPoJo`
    ```java
    public class Foo extends AbstractEntityPoJo{}
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
## `wind-demo`模块提供所有方法的使用示例
## controller层
```java
@Resource private FooService service;

@GetMapping(value = "/detail/{id}", params = "type=2")
@ApiOperation("查询,根据id获取详情,并转换为response")
public ResponseEntity<Rs<FooResponse>> detail2(@PathVariable("id") final long id) {
    return ControllerHelper.detail(service, id, FooResponse::of);
}

@GetMapping(value = "/list")
@ApiOperation("查询,列表查询,支持转换和过滤")
public ResponseEntity<Rs<List<FooResponse>>> list() {
    final List<Foo> list = service.list(condition -> condition.like(Foo::setName, "foo"));
    return ControllerHelper.list(list, FooResponse::of, foo -> StringUtils.nonEmpty(foo.getName()));
}

@GetMapping(value = "/page")
@ApiOperation("查询,分页")
public ResponseEntity<Rs<Page<FooResponse>>> page() {
    // page需要自己调用分页查询,仅提供相关的对象转换方法
    final Page<Foo> page =
        service.page(
            condition -> condition.eq(Foo::setName, "foo"),
            1,
            10,
            SortColumn.by(Foo::getUpdateTime, Order.DESC).desc(Foo::getId));
    return ControllerHelper.page(page, FooResponse::of);
}
```
## service层
```java
@Resource private FooService service;

@GetMapping(value = "/get-one", params = "type=2")
@ApiOperation("查询,单条查询,指定条件返回自定义对象,支持返回基本类型")
public ResponseEntity<Rs<FooThinResponse>> getOne2() {
    // 支持返回基本类型
    final Long one =
        service.getOne(
            query -> query.col(Foo::getId),
            condition -> condition.eq(Foo::setId, 1L),
            Long.class);
    log.info("getOne2:[{}]", one);
    // 返回自定义对象
    final FooThinResponse thinResponse =
        service.getOne(
            query ->
                query
                    .col(Foo::getId)
                    .col(Foo::getName)
                    .col(Foo::getCreateTime),
            condition -> condition.eq(Foo::setId, 1L),
            FooThinResponse.class);
    log.info("getOne2:[{}]", thinResponse);
    return Rs.ok(thinResponse);
}

@PostMapping(value = "/create", params = "type=2")
@SuppressWarnings({"unchecked", "DuplicatedCode"})
@ApiOperation("创建,指定保存值为null的属性")
public ResponseEntity<Rs<Long>> create2() {
    return Rs.ok(
        service.create(
            Foo.builder()
                .name("demo")
                .textString("text")
                .bigDecimal(BigDecimal.valueOf(100))
                .type(Type.SPORT)
                .intList(Arrays.asList(1, 3, 5))
                .intArr(new Integer[] {1, 4, 7})
                .longList(Arrays.asList(2L, 4L, 6L))
                .longArr(new Long[] {1L, 3L, 5L})
                // .stringList(Arrays.asList("3", "a", "6", "b"))
                .stringArr(new String[] {"2", "a", "b"})
                .column("non_match_column")
                .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                .build(),
            Foo::getName,
            Foo::getStringList));
}

```
## repository层(Query/Update)
支持任意表
```java
@Resource private PrototypeBean prototypeBean;

@GetMapping
@ApiOperation("使用Query")
public ResponseEntity<Rs<Object>> query() {
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
public ResponseEntity<Rs<Object>> update() {
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

## 自定义枚举序列化
 - 默认枚举序列化为非线性key:value格式
    ```json
    {
        "value": "值",
        "desc": "中文描述"
    }
    ```
 - 自定义
    ```java
    /**
    * 自定义枚举的序列化格式,只返回value.
    *
    * @return the inter enum serializer
    */
    @Bean
    public InterEnumSerializer interEnumSerializer() {
        return InterEnum::value;
    }
    ```

## 自定义ID生成策略
默认使用分布式id雪花算法
```java
@Bean
public IdGenerator autoIncrementGenerator() {
    // 自定义id生成策略,下方为数据库自增写法
    return o -> null;
}
```

## 频繁请求拦截
```yaml
wind:
  frequency-request-intercept:
    # 默认开启
    enable: true
    # 请求最小时差,单位ms,默认150ms
    time: 200
```

## redis分布式缓存
```yaml
  redis-cache:
    # 默认开启
    enable: true
    # 前缀
    key-prefix: io.github.ramerf.wind
```