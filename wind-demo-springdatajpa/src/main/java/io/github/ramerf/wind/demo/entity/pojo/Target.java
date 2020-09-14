package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.demo.entity.domain.ProductCategory;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Target {
  public static List<ProductCategory> getCategories() {
    log.info("getCategories:[{}]", "===================");
    return Collections.emptyList();
  }
}
