package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.config.PrototypeBean.QueryUpdateRegister;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.annotation.Lookup;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.boot.autoconfigure.AutoConfigurationPackages;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.*;
import org.springframework.core.annotation.Order;
import org.springframework.core.type.AnnotationMetadata;

/**
 * 该类用于管理prototype类型的bean,由于违背Ioc,不建议使用{@link AppContextInject}获取bean.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/29
 */
@Slf4j
@Order(2)
@Configuration
@Import(QueryUpdateRegister.class)
@ConditionalOnMissingBean({Query.class, Update.class})
public class PrototypeBean {
  @Lookup
  public <T extends AbstractEntityPoJo<T, ?>> Query<T> query(Class<T> clazz) {
    return null;
  }

  @Lookup
  public <T extends AbstractEntityPoJo<T, ?>> Update<T> update(Class<T> clazz) {
    return null;
  }

  public static class QueryUpdateRegister
      implements BeanFactoryAware, ImportBeanDefinitionRegistrar {

    private BeanFactory beanFactory;

    @Override
    public void registerBeanDefinitions(
        @Nonnull AnnotationMetadata annotationMetadata, @Nonnull BeanDefinitionRegistry registry) {
      if (!AutoConfigurationPackages.has(this.beanFactory)) {
        return;
      }
      BeanDefinitionBuilder queryDefinition =
          BeanDefinitionBuilder.genericBeanDefinition(Query.class);
      queryDefinition.setScope(ConfigurableBeanFactory.SCOPE_PROTOTYPE);
      registry.registerBeanDefinition(Query.class.getName(), queryDefinition.getBeanDefinition());

      BeanDefinitionBuilder updateDefinition =
          BeanDefinitionBuilder.genericBeanDefinition(Update.class);
      updateDefinition.setScope(ConfigurableBeanFactory.SCOPE_PROTOTYPE);
      registry.registerBeanDefinition(Update.class.getName(), updateDefinition.getBeanDefinition());
    }

    @Override
    public void setBeanFactory(@Nonnull BeanFactory beanFactory) {
      this.beanFactory = beanFactory;
    }
  }
}
