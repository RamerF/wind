// package io.github.ramerf.wind.core.config;
//
// import io.github.ramerf.wind.core.entity.enums.InterEnum;
// import io.github.ramerf.wind.core.executor.Executor;
// import io.github.ramerf.wind.core.executor.SimpleJdbcExecutor;
// import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
// import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerRegistryFactory;
// import io.github.ramerf.wind.core.ioc.Bean;
// import io.github.ramerf.wind.core.serializer.InterEnumSerializer;
// import io.github.ramerf.wind.core.support.IdGenerator;
// import java.util.LinkedList;
// import java.util.List;
// import javax.annotation.PostConstruct;
// import javax.sql.DataSource;
// import lombok.extern.slf4j.Slf4j;
//
// /**
//  * 定义常用bean.
//  *
//  * @author ramer
//  * @since 2019 /12/29
//  */
// @Slf4j
// @Bean
// public class CommonBean {
//   @SuppressWarnings({"rawtypes", "SpringJavaAutowiredFieldsWarningInspection"})
//   private final List<ITypeHandler> typeHandlers = new LinkedList<>();
//
//   /** 注册类型转换器. */
//   @PostConstruct
//   public void postConstruct() {
//     // TODO WARN 注册类型转换器
//     TypeHandlerRegistryFactory.addTypeHandlers(typeHandlers);
//   }
//
//   @Bean
//   public Executor jdbcTemplateExecutor(DataSource dataSource) {
//     return new SimpleJdbcExecutor();
//   }
//
//   /** id默认自增. */
//   @Bean
//   public IdGenerator autoIncrementIdGenerator() {
//     return o -> null;
//   }
//
//   /** 枚举默认使用value方法序列化. */
//   @Bean
//   public InterEnumSerializer interEnumSerializer() {
//     return InterEnum::value;
//   }
// }
