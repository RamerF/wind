package io.github.ramerf.wind.demo.repository;

import io.github.ramerf.wind.demo.entity.domain.Product;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/17
 */
@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {}
