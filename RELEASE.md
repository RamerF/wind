# 版本日志

---

#### 4.0.2-RELEASE

- 新增：更新指定字段
- 新增：查询支持
- 新增：字符串条件构造`StringCondition`。如：`condition.eq("id", 1);`
- 新增：主键支持任意类型
- 新增：条件组，拼接or条件更清晰
- 新增：`GenericService`支持操作任意表
- 新增：支持内存缓存。通过`wind.cache.type=memory`开启
- 新增：实体现在自带保存/更新方法 `product.create();`
- 新增：枚举校验注解
- 新增：校验器工具类
- 新增：全局异常处理
- 更新：默认关闭缓存
- 更新：默认关闭逻辑删除
- 更新：默认写入所有(包括null值)属性，通过`wind.write-null-prop=true/false`开关
- 更新：InterEnum枚举值由整型改为泛型。controller枚举支持接收名称和value值
- 更新：@Column替换为@TableColumn
- 更新：弃用@Entity，使用@TableInfo
- 更新：其他易用性更新
- 优化：缓存清除时改为使用scan指令
- 修复：字段为boolean时，lambda无法获取到field
- 修复：缓存没有正确清除

#### 4.0.1-RELEASE

- 新增：缓存支持自定义扩展，目前支持内存和Redis，默认使用内存缓存
- 新增：校验器工具类，用于支持手动校验和获取校验错误信息
- 新增：支持controller接收枚举参数并自动校验有效性
- 新增：全局异常处理
- 修复：缓存没有正确清除
- 优化：清除`redis`缓存时改为使用scan指令

#### 4.0.0-RELEASE

- 新增：自动建表。支持表，列备注
- 新增：查询支持排除大字段（可以理直气壮的偷懒，去查询整个对象了😁）
- 新增：支持禁用公共字段（deleted，create_time，update_time），禁用后字段不再映射到数据库
- 新增：启动完成后发布事件，可以通过监听事件执行额外操作
- 修复：写入操作时，创建/更新时间未正确赋值
- 修复：逻辑删除字段未正确赋值
- 修复：分页查询缓存异常

#### 3.8.3-RELEASE

- 新增：清除缓存

```java
@Resource private Cache cache;
cache.clear(Foo.class);
```

- 新增：字段支持使用TypeHandler注解指定类型处理器

```java
@TypeHandler(XxxHandler.class)
private Type type;
```

- 新增：与spring-data-jpa结合示例
- 更新：类型处理器移动到handler/typehandler下
- 修复：注入顺序错误导致整合应用启动失败

#### 3.8.0-RELEASE

- 添加：查询缓存支持（redis）
- 添加：自定义查询结果转换器
- 添加：自定义枚举序列化
- 修复：Feign调用时，枚举反序列化异常