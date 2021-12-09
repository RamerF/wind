package io.github.ramerf.wind.core.metadata;

import java.util.*;
import java.util.stream.Collectors;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

/**
 * 数据库表信息.
 *
 * @since 2021.09.20
 * @author ramer
 */
@Slf4j
@Data
public class TableIndexInformation {
  private String name;
  private boolean unique;
  private short type;
  private String columnName;
  /** 值为: A,D或空 */
  private String order;

  private String tableName;

  public static List<TableIndexInformation> of(final Set<FlatTableIndexInformation> informations) {
    List<TableIndexInformation> indexInformations = new ArrayList<>();
    informations.stream()
        .collect(Collectors.groupingBy(FlatTableIndexInformation::getName))
        .forEach(
            (name, v) -> {
              TableIndexInformation indexInformation = new TableIndexInformation();
              indexInformation.name = name;
              final FlatTableIndexInformation first = v.get(0);
              indexInformation.unique = first.unique;
              indexInformation.type = first.type;
              indexInformation.columnName =
                  v.size() == 1
                      ? first.columnName
                      : v.stream()
                          .map(idx -> idx.columnName + " " + idx.order)
                          .collect(Collectors.joining(","));
              indexInformation.tableName = first.tableName;
              indexInformations.add(indexInformation);
            });
    return indexInformations;
  }

  /** 根据名称匹配. */
  @Override
  public boolean equals(final Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    final TableIndexInformation that = (TableIndexInformation) o;
    return Objects.equals(name, that.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @ToString
  public static class FlatTableIndexInformation implements Comparable<FlatTableIndexInformation> {
    @Getter private String name;
    private boolean unique;
    private short type;
    private String columnName;
    private short position;
    /** 值为: A,D或空 */
    private String order;

    private String tableName;

    public static FlatTableIndexInformation of(
        final String name,
        final boolean unique,
        final short type,
        final String columnName,
        final short position,
        final String order,
        final String tableName) {
      FlatTableIndexInformation information = new FlatTableIndexInformation();
      information.name = name;
      information.unique = unique;
      information.type = type;
      information.columnName = columnName;
      information.position = position;
      information.order = order;
      information.tableName = tableName;
      log.trace("[{}]", information);
      return information;
    }

    @Override
    public int compareTo(final FlatTableIndexInformation o) {
      return position - o.position;
    }
  }
}
