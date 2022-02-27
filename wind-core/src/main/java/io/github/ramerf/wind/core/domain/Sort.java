package io.github.ramerf.wind.core.domain;

import io.github.ramerf.wind.core.domain.Sort.Order;
import io.github.ramerf.wind.core.util.Asserts;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.Serializable;
import java.util.*;
import java.util.stream.*;
import javax.annotation.Nullable;
import lombok.NonNull;

public class Sort implements Serializable, Iterable<Order> {
  private static final Sort UNSORTED = Sort.by(new Order[0]);
  public static final Direction DEFAULT_DIRECTION = Direction.ASC;
  private final List<Order> orders;

  public Sort(String... properties) {
    this(DEFAULT_DIRECTION, properties);
  }

  public Sort(Order... orders) {
    this(Arrays.asList(orders));
  }

  public Sort(List<Order> orders) {
    Asserts.notNull(orders, "Orders must not be null!");
    this.orders = Collections.unmodifiableList(orders);
  }

  public Sort(Direction direction, String... properties) {
    this(direction, properties == null ? new ArrayList<>() : Arrays.asList(properties));
  }

  public Sort(Direction direction, List<String> properties) {
    if (properties == null || properties.isEmpty()) {
      throw new IllegalArgumentException("You have to provide at least one property to sort by!");
    }

    this.orders = new ArrayList<>(properties.size());

    for (String property : properties) {
      this.orders.add(new Order(direction, property));
    }
  }

  public Stream<Order> stream() {
    return StreamSupport.stream(spliterator(), false);
  }

  public static Sort by(String... properties) {
    Asserts.notNull(properties, "Properties must not be null!");
    return properties.length == 0 ? Sort.unsorted() : new Sort(properties);
  }

  public static Sort by(List<Order> orders) {
    Asserts.notNull(orders, "Orders must not be null!");
    return orders.isEmpty() ? Sort.unsorted() : new Sort(orders);
  }

  public static Sort by(Order... orders) {
    Asserts.notNull(orders, "Orders must not be null!");
    return new Sort(orders);
  }

  public static Sort by(@NonNull Direction direction, @NonNull String... properties) {
    Asserts.isTrue(properties.length > 0, "At least one property must be given!");
    return Sort.by(
        Arrays.stream(properties) //
            .map(it -> new Order(direction, it)) //
            .collect(Collectors.toList()));
  }

  public static Sort unsorted() {
    return UNSORTED;
  }

  public Sort and(Sort sort) {
    Asserts.notNull(sort, "Sort must not be null!");
    ArrayList<Order> these = new ArrayList<>(this.orders);
    for (Order order : sort) {
      these.add(order);
    }
    return Sort.by(these);
  }

  @Override
  public Iterator<Order> iterator() {
    return this.orders.iterator();
  }

  @Override
  public boolean equals(@Nullable Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof Sort)) {
      return false;
    }
    Sort that = (Sort) obj;
    return this.orders.equals(that.orders);
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + orders.hashCode();
    return result;
  }

  @Override
  public String toString() {
    return orders.isEmpty()
        ? "UNSORTED"
        : orders.stream().map(Order::toString).collect(Collectors.joining(","));
  }

  public enum Direction {
    ASC,
    DESC;

    public boolean isAscending() {
      return this.equals(ASC);
    }

    public boolean isDescending() {
      return this.equals(DESC);
    }

    public static Direction fromString(String value) {
      try {
        return Direction.valueOf(value.toUpperCase(Locale.US));
      } catch (Exception e) {
        throw new IllegalArgumentException(
            String.format(
                "Invalid value '%s' for orders given! Has to be either 'desc' or 'asc' (case insensitive).",
                value),
            e);
      }
    }
  }

  public static class Order implements Serializable {
    private static final boolean DEFAULT_IGNORE_CASE = false;
    private final Direction direction;
    private final String property;
    private final boolean ignoreCase;

    public Order(String property) {
      this(DEFAULT_DIRECTION, property);
    }

    public Order(@Nullable Direction direction, String property) {
      this(direction, property, DEFAULT_IGNORE_CASE);
    }

    public static Order by(String property) {
      return new Order(DEFAULT_DIRECTION, property);
    }

    public static Order asc(String property) {
      return new Order(Direction.ASC, property);
    }

    public static Order desc(String property) {
      return new Order(Direction.DESC, property);
    }

    private Order(@Nullable Direction direction, String property, boolean ignoreCase) {
      if (!StringUtils.hasText(property)) {
        throw new IllegalArgumentException("Property must not null or empty!");
      }

      this.direction = direction == null ? DEFAULT_DIRECTION : direction;
      this.property = property;
      this.ignoreCase = ignoreCase;
    }

    public Direction getDirection() {
      return direction;
    }

    public String getProperty() {
      return property;
    }

    public boolean isAscending() {
      return this.direction.isAscending();
    }

    public boolean isDescending() {
      return this.direction.isDescending();
    }

    @Override
    public String toString() {
      return String.format("%s: %s", property, direction);
    }
  }
}
