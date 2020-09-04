#  版本日志

---
#### 4.0.1-RELEASE

- 新增：枚举校验注解
- 新增：校验器工具类
- 新增：全局异常处理
- ...

#### 4.0.0-RELEASE

- 新增：自动建表。支持表，列备注
- 新增：查询支持排除大字段（可以理直气壮的偷懒，去查询整个对象了😁）
- 新增：支持禁用公共字段（deleted，create_time，update_time），禁用后字段不再映射到数据库
- 新增：启动完成后发布事件，可以通过监听事件执行额外操作
- 修复：写入操作时，创建/更新时间未正确赋值
- 修复：逻辑删除字段未正确赋值
- 修复：分页查询缓存异常

#### 3.8.3-RELEASE

- 新增：清除redis缓存，用于其它orm框架执行对象写入后的缓存清除

  ```java
  @Resource private RedisCache redisCache;
  redisCache.clear(Foo.class);
  ```

- 新增：字段支持使用TypeHandler注解指定类型处理器

  ```java
  @TypeHandler(EnumTypeHandler.class)
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