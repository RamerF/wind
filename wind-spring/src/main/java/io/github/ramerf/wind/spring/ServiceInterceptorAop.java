package io.github.ramerf.wind.spring;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.plugin.Invocation;
import io.github.ramerf.wind.core.plugin.ServiceInterceptorChain;
import io.github.ramerf.wind.core.service.BaseService;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;

/**
 * 拦截BaseService中的方法.
 *
 * @author ramer
 * @since 2022.06.06
 */
@Slf4j
@Aspect
public class ServiceInterceptorAop {
  private final Configuration configuration;

  public ServiceInterceptorAop(Configuration configuration) {
    this.configuration = configuration;
  }

  @Around("target(io.github.ramerf.wind.core.service.BaseService)")
  public Object proceed(final ProceedingJoinPoint pjp) throws Throwable {
    Signature signature = pjp.getSignature();
    MethodSignature methodSignature = null;
    final Object target = pjp.getTarget();
    if (!(signature instanceof MethodSignature) || !(target instanceof BaseService)) {
      return pjp.proceed();
    }
    @SuppressWarnings({"rawtypes"})
    final BaseService baseService = (BaseService) target;
    final ServiceInterceptorChain interceptorChain =
        configuration.getServiceInterceptorChain().support(baseService.getPoJoClass());
    if (interceptorChain.getInterceptors().isEmpty()) {
      return pjp.proceed();
    }
    return interceptorChain.proceed(
        new Invocation(
            baseService,
            ((MethodSignature) signature).getMethod(),
            pjp.getArgs(),
            interceptorChain));
  }
}
