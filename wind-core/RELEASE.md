# ratel-turbo-core 版本日志

---

## mybatis增强插件,支持功能

### controller层
 - [X] 新增: 新增PoJo,Request
 - [X] 新增: 新增Request,支持companyId
 - [X] 查询: 通过id获取详情(该方法会查询所有字段,支持转换为任意对象)
 - [X] 更新: 更新PoJo,不带校验器
 - [X] 更新: 更新PoJo,带校验器
 - [X] 更新: 更新Request
 - [X] 更新: 更新Request,带companyId
 - [X] 更新: 批量更新Request
 - [X] 删除: 通过id删除
 - [X] 删除: 通过id和companyId删除
 - [X] 删除: 通过id集合批量删除
 - [X] 删除: 通过id集合和companyId批量删除
 - [X] 删除: 包含返回值,自定义返回信息
 - [X] 删除: 不包含返回值,自定义返回信息
 - [X] 工具: 转换并过滤list
 - [X] 工具: 转换list为page对象
 - [X] 工具: 转换并过滤list为page对象
 - [X] 工具: 获取校验信息的第一条错误信息
 - [X] 工具: 获取校验信息的所有错误信息
 - [X] 工具: Controller枚举参数校验
### service层
 - [X] 单表通用增删改查,分页查询
 - [X] 单表批量增删改
 - [X] 单表查询指定字段,支持分页
 - [X] 多表通用查询,支持分页
 - [X] 多表查询指定字段,支持分页
 - [X] 单表查询某页列表数据,结果不分页
 - [X] 分布式缓存支持
### repository层
 - [X] 单表通用增删改查,分页查询
 - [X] 单表批量增删改
 - [X] 单表查询指定字段,支持分页
 - [X] 多表通用查询,支持分页
 - [X] 多表查询指定字段,支持分页
 - [ ] 分页查询:包含指定数据且在首位
 - [ ] 单表查询常用统计函数支持
 - [ ] 多表查询常用统计函数支持
 - [X] 分布式缓存支持
### entity层
 - [X] request转pojo
### 代码生成器
 - [X] 单表通用增删改查,~~不支持带企业id~~
 - [ ] 单表新增支持带企业id
 - [ ] 单表查询支持带企业id
 - [ ] 单表更新支持带企业id
 - [ ] 单表删除支持带企业id
### 功能计划
 - [ ] 查询缓存支持
 - [ ] 逐步启用BaseService#ExtraProp查询方式,前提:查询缓存支持
### 后期新增功能根据业务调整.

### v1.0.0-SNAPSHOT 2020-02-25
 - 新增
    1. 单表通用增删改查,分页查询
    2. 单表批量增删改
    3. 单表查询指定字段,支持分页
    4. 多表通用查询,支持分页
    5. 多表查询指定字段,支持分页
    6. Controller枚举参数校验支持
### v1.0.0-SNAPSHOT 2020-03-04
 - 新增: 
    - entity三层支持枚举,request支持自动校验
### v1.0.0-SNAPSHOT 2020-03-13
 - 新增
    - controller层 更新: 更新PoJo,不带校验器
    - service层 查询: 单表查询某页列表数据,结果不分页
### v1.0.0-SNAPSHOT 2020-03-17
 - 新增
    - controller层 更新: 创建PoJo,不带校验器
### v1.0.0-SNAPSHOT 2020-03-24
 - 新增
    - AppContextInject通过Bean名称获取Bean
    - 分布式缓存支持
    - 分布式缓存DEMO
 - 更新
    - Page转换工具类独立出来
 - 修复
    - Mybatis自定义转换器Integer[]/Long[] <-> List\<Integer>/List\<Long>的不确定性导致查询/保存数据丢失.
    - Conditions条件notIn语法错误
    - 修复BeanResultHandler当返回数据的枚举值为null时会执行失败
### v1.0.0-SNAPSHOT 2020-04-02
 - 新增
    - ControllerHelper新增两个删除方法
### v1.0.0-SNAPSHOT 2020-04-05
 - 新增
    - 类型转换器List<Long> <-> Long[]
    - 类型转换器List<String> <-> String[]
    - 非单例Bean管理器
 - 优化   
    - 现在查询字段通过返回对象推断,减少了传输数据
 - 重构
    - 类型转换器现在兼容性更好了
 - 修复
    - 修复了一个潜在的bug,该bug会导致优惠券相关功能在并发环境下一定概率执行失败
 - 更新
    - 修正ApplicationContext注入方式,更新说明文档
 - 其它更新
### v2.0.0-SNAPSHOT 2020-04-14
 - 新增
    - 查询方法支持返回基本类型
    - CollectionUtils新增两个toPage方法
    - 条件删除
    - ResCode业务响应对象
    - DEMO
 - 重构
    - 重构service方法签名,现在使用更方便了
### v2.0.2-SNAPSHOT 2020-04-27
 - 新增
    - service层count支持条件(暂不支持列)
    - service查询,支持返回基本类型数组
    
### v2.0.2-SNAPSHOT 2020-05-05
 - 重构
    - 项目名称+结构
 - 新增
    - 包注释
    - 测试项目
    - 部分函数支持(待测试)
### 进行中
 - [ ] 批量新增性能优化,待测试
