/*
 * Copyright 2002-2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.ramerf.wind.core.ioc;

import java.lang.annotation.*;

/**
 * 标识类为一个可自动管理的bean.
 *
 * @author ramer
 * @since 2021.12.25
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Bean {

  /** {@link #name}的别名 */
  String value() default "";

  /** 被自动管理bean的名称,默认为类名的驼峰形式 */
  String name() default "";

  /** 首要的 */
  boolean primary() default false;

  /** 单例bean */
  boolean singleton() default true;

  /** 所有bean默认构造器执行后的实例化顺序,数值越小越早 */
  int getOrder() default Integer.MAX_VALUE;

  /** 注入类型 */
  InjectType injectType() default InjectType.DEFAULT;

  enum InjectType {
    /** 默认先根据类型然后名称 */
    DEFAULT,
    /** 仅根据类型 */
    BY_TYPE,
    /** 仅根据名称 */
    BY_NAME
  }
}
