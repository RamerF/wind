package io.github.ramerf.mybatisturbo.core.conditions;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.mybatisturbo.core.config.AppContextInject;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.constant.Constant;
import io.github.ramerf.mybatisturbo.core.entity.response.ResultCode;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.handler.*;
import io.github.ramerf.mybatisturbo.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.mybatisturbo.core.repository.AbstractBaseRepository;
import io.github.ramerf.mybatisturbo.core.util.*;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.*;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import static io.github.ramerf.mybatisturbo.core.conditions.Predicate.SqlOperator.*;
import static io.github.ramerf.mybatisturbo.core.helper.SqlHelper.optimizeQueryString;

/**
 * 构建 SQL.所有的条件字符串构造,需要改为对象<br>
 * 如: OrdrByClause... <br>
 * TODO: 查询缓存
 *
 * <p>下面是新的实现思路 <br>
 * select {@link SqlFunction} <br>
 * from table1 left join table2<br>
 * where {@link SqlFunction} <br>
 * {@link SqlFunction}
 *
 * <p>当{@link QueryAlia#sqlFunction}为null时,退化为普通条件,否则就是函数
 *
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
@Slf4j
@Component
@SuppressWarnings("all")
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class Query {
  /*
   TODO-TXF: 添加支持: 查询包含指定数据(可能是多个)的分页数据,并置于首位
   思路: 构造OrderByClause,使用sql语法:
   1. orderby id <> ?
   2. orderby id not in (?,?) 特性
   3. orderby case when id=? then min_number else max_number end
  */
  @Resource private AbstractBaseRepository repository;
  private List<QueryColumn<?>> queryColumns;
  private List<Conditions<?>> conditions;
  private String queryString;
  private String conditionString;
  private String countString;
  private String orderByString;
  /** 暂时用于where之后的函数(group by等). */
  private final StringBuilder suffixString = new StringBuilder();

  public static Query getInstance() {
    final Query bean = AppContextInject.getBean(Query.class);
    log.info("getInstance:[{}]", bean);
    return bean;
  }

  public Query select(final QueryColumn<?>... queryColumns) {
    this.queryColumns = new LinkedList<>(Arrays.asList(queryColumns));
    this.queryString =
        this.queryColumns.stream()
            .map(QueryColumn::getString)
            .collect(Collectors.joining(Constant.SEMICOLON));
    return this;
  }

  public Query from(final Conditions<?>... conditions) {
    // TODO-WARN: from方法
    throw CommonException.of("方法未实现");
  }

  public Query where(final Conditions<?>... conditions) {
    this.conditions = new LinkedList<>(Arrays.asList(conditions));
    String conditionString =
        this.conditions.stream()
            .map(Conditions::getString)
            .collect(Collectors.joining(AND.operator()));

    if (conditionString.endsWith(AND.operator())) {
      conditionString =
          conditionString.substring(0, conditionString.length() - AND.operator().length());
    }
    this.conditionString =
        this.conditions.stream()
            .map(o -> o.queryEntityMetaData.getFromTable())
            .collect(Collectors.joining(SEMICOLON.operator()));
    // TODO-WARN 修复orderBy从这里下手
    // 每个条件带表别名
    // 定义orderByString ,这里赋值

    if (StringUtils.nonEmpty(conditionString)) {
      this.conditionString = this.conditionString.concat(WHERE.operator()).concat(conditionString);
    }
    if (StringUtils.nonEmpty(this.conditionString)) {
      this.countString = this.conditionString;
    }
    return this;
  }

  public Query groupBy(@Nonnull final GroupByClause... groupByClauses) {
    suffixString
        .append(GROUP_BY)
        .append(
            Stream.of(groupByClauses)
                .flatMap(groupByClause -> groupByClause.getCols().stream())
                .collect(Collectors.joining()));
    return this;
  }

  // TODO-WARN 这里可以做优化,调用插件分析优化sql,最简单的比如: 删掉 1=1
  public Query where(final Consumer<Conditions<?>>... consumers) {
    this.conditions =
        consumers.length > 0
            ? IntStream.range(0, consumers.length)
                .mapToObj(
                    i -> {
                      Optional.ofNullable(consumers[i])
                          .ifPresent(
                              consumer -> consumer.accept(queryColumns.get(i).getConditions()));
                      return queryColumns.get(i).getConditions();
                    })
                .collect(Collectors.toCollection(LinkedList::new))
            : new LinkedList<>();
    String conditionString =
        this.conditions.stream()
            .map(Conditions::getString)
            .collect(Collectors.joining(AND.operator()));

    if (conditionString.endsWith(AND.operator())) {
      conditionString =
          conditionString.substring(0, conditionString.length() - AND.operator().length());
    }
    this.conditionString =
        this.conditions.stream()
            .map(o -> o.queryEntityMetaData.getFromTable())
            .collect(Collectors.joining(SEMICOLON.operator()));
    if (StringUtils.nonEmpty(conditionString)) {
      this.conditionString = this.conditionString.concat(WHERE.operator()).concat(conditionString);
    }
    return this;
  }

  public <R> R fetchOne(final Class<R> clazz) {
    ResultHandler resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz, queryColumns)
            : new BeanResultHandler<>(clazz, queryColumns);
    // TODO-WARN: 待测试
    StringUtils.doIfNonEmpty(
        str -> {
          conditionString = conditionString.concat(str);
        },
        suffixString.toString());
    // TODO-WARN: 待测试
    final Map<String, Object> result =
        repository.findOne(optimizeQueryString(queryString, clazz), conditionString);
    return (R) resultHandler.handle(result);
  }

  public <R> List<R> fetch(final Class<R> clazz) {
    // TODO-WARN: 待测试
    StringUtils.doIfNonEmpty(
        str -> {
          conditionString = conditionString.concat(str);
        },
        suffixString.toString());
    // TODO-WARN: 待测试
    final List<Map<String, Object>> list =
        repository.findAll(optimizeQueryString(queryString, clazz), conditionString);
    if (CollectionUtils.isEmpty(list)) {
      return Collections.emptyList();
    }
    ResultHandler resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz, queryColumns)
            : new BeanResultHandler<>(clazz, queryColumns);
    return resultHandler.handle(list);
  }

  /** 获取某页列表数据. 注意: 该方法不支持多表查询. */
  public <R extends AbstractEntity> List<R> fetch(
      final Class<R> clazz, final PageRequest pageable) {
    final Sort sort = pageable.getSort();
    // TODO-WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        sort.stream()
            .map(
                s ->
                    s.getProperty()
                        .concat(Constant.DEFAULT_SPLIT_SPACE)
                        .concat(s.getDirection().name()))
            .collect(Collectors.joining(Constant.SEMICOLON));
    conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    if (log.isDebugEnabled()) {
      log.debug("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }
    final List<Map<String, Object>> list =
        repository.findAllPage(optimizeQueryString(queryString, clazz), conditionString, pageable);
    if (log.isDebugEnabled()) {
      log.debug("fetch:[{}]", list);
    }
    ResultHandler resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz, queryColumns)
            : new BeanResultHandler<>(clazz, queryColumns);
    return resultHandler.handle(list);
  }

  /** 注意: 该方法不支持多表查询. */
  public <R> Page<R> fetch(final PageRequest pageable, final Class<R> clazz) {
    final Sort sort = pageable.getSort();
    // TODO-WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        sort.stream()
            .map(
                s ->
                    s.getProperty()
                        .concat(Constant.DEFAULT_SPLIT_SPACE)
                        .concat(s.getDirection().name()))
            .collect(Collectors.joining(Constant.SEMICOLON));
    conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    if (log.isDebugEnabled()) {
      log.debug("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }
    final List<Map<String, Object>> list =
        repository.findAllPage(optimizeQueryString(queryString, clazz), conditionString, pageable);
    // 从1开始
    final int currentPage = pageable.getPageNumber();
    // 每页大小
    final int pageSize = pageable.getPageSize();
    if (CollectionUtils.isEmpty(list)) {
      return CollectionUtils.toPage(Collections.emptyList(), 0, currentPage, pageSize);
    }
    ResultHandler resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz, queryColumns)
            : new BeanResultHandler<>(clazz, queryColumns);
    return CollectionUtils.toPage(resultHandler.handle(list), fetchCount(), currentPage, pageSize);
  }

  // TODO-ZRH 此处有问题
  public long fetchCount() {
    if (log.isDebugEnabled()) {
      log.debug("fetchCount:[queryString:{},countString:{}]", queryString, countString);
    }
    return repository.count(StringUtils.nonEmpty(countString) ? countString : "");
  }

  // TODO-WARN 根据条件批量删除,批量更新
  public boolean deleteBatch() {
    throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
  }
}
