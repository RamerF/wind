package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.condition.Cnds;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.mysql.Foo.Type;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.*;
import java.math.BigDecimal;
import java.net.URL;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.core.annotation.Order;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.transaction.annotation.Transactional;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.reader.UnicodeReader;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Service层测试.
 *
 * @author ramer
 * @since 2020.8.29
 */
@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Mysql 测试")
public class BaseServiceTest {
  private GenericService<Foo, Long> service;
  private static final Foo foo;
  private static final Long id = 10000L;

  static {
    foo =
        Foo.builder()
            .id(1L)
            .name("test")
            .textString("textString")
            .bigDecimal(BigDecimal.valueOf(100))
            .type(Type.SPORT)
            .column("non_match_column")
            .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
            // .bigText("")
            .isNumber(false)
            .isNull(false)
            .string(true)
            .nonNull(true)
            .typeJson(Type.SPORT)
            .typesJson(Arrays.asList(Type.PHONE, Type.SPORT))
            .createTime(LocalDateTime.now())
            .build();
  }

  @BeforeEach
  public void before() {
    try {
      Yaml yaml = new Yaml();
      URL url = BaseServiceTest.class.getClassLoader().getResource("application-mysql.yml");
      if (url != null) {
        // 获取test.yaml文件中的配置数据，然后转换为obj，
        Object obj = yaml.load(new FileInputStream(url.getFile()));
        System.out.println(obj);
        // 也可以将值转换为Map
        final FileInputStream inputStream = new FileInputStream(url.getFile());
        Configuration configuration = new Configuration();
        final Map<String, Object> map2 = yamlHandler(new FileInputStream[] {inputStream});
        System.out.println(map2);
        // 通过map我们取值就可以了.

      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    foo.setId(id);
    service = GenericService.with(Foo.class, Long.class);
  }

  /** 单个yaml文件处理 */
  public static Map<String, Object> yamlHandler(@Nonnull InputStream[] resources)
      throws IOException {
    Map<String, Object> result = new LinkedHashMap<>();
    Yaml yaml = new Yaml();
    Iterator<InputStream> iterator = Arrays.stream(resources).iterator();
    while (iterator.hasNext()) {
      InputStream resource = iterator.next();
      UnicodeReader reader = new UnicodeReader(resource);
      Object object = yaml.load(reader);
      // 这里只是简单处理，需要多个方式可以自己添加
      if (object instanceof Map) {
        Map map = (Map) object;
        buildFlattenedMap(result, map, null);
      }
      reader.close();
    }
    return result;
  }

  /** 这部分代码来至springboot源码部分对yaml的解析 YamlProcessor.java buildFlattenedMap方法 */
  private static void buildFlattenedMap(
      Map<String, Object> result, Map<String, Object> source, @Nullable String path) {
    // 循环读取原数据
    source.forEach(
        (key, value) -> {
          // 如果存在路径进行拼接
          if (StringUtils.hasText(path)) {
            if (key.startsWith("[")) {
              key = path + key;
            } else {
              key = path + '.' + key;
            }
          }
          // 数据类型匹配
          if (value instanceof String) {
            result.put(key, value);
          } else if (value instanceof Map) {
            // 如果是map,就继续读取
            Map<String, Object> map = (Map) value;
            buildFlattenedMap(result, map, key);
          } else if (value instanceof Collection) {
            Collection<Object> collection = (Collection) value;
            if (collection.isEmpty()) {
              result.put(key, "");
            } else {
              int count = 0;
              Iterator var7 = collection.iterator();
              while (var7.hasNext()) {
                Object object = var7.next();
                buildFlattenedMap(
                    result, Collections.singletonMap("[" + count++ + "]", object), key);
              }
            }
          } else {
            result.put(key, value != null ? value : "");
          }
        });
  }

  @Test
  @DisplayName("条件构造工具类")
  public void testCnds() {
    final Cnds<Foo> cnds =
        // 指定操作的表
        Cnds.of(Foo.class)
            // 条件
            .gt(Foo::setId, 0L)
            // 自定义sql
            .and("name is not null")
            // 分页
            .limit(1, 10)
            // 分组
            .groupBy(Foo::getName)
            // 排序
            .orderBy(Foo::getId, Direction.DESC);
    log.info("testCnds:[{}]", cnds.getString());
    final Fields<Foo> fields =
        Fields.of(Foo.class)
            // 查询指定列
            .include(Foo::getId)
            .include(Foo::getName);
    log.info(
        "testCnds:[include:{},exlucde:{}]", fields.getIncludeFields(), fields.getExcludeFields());
  }

  @Test
  @DisplayName("统计")
  public void testCount() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L);
    assertTrue(service.count(cnds) > 0);
  }

  @Test
  @DisplayName("查询单个")
  public void testGetOne() {
    // 通过id查询
    assertNotNull(service.getOne(id));
    // 条件查询
    assertNotNull(service.getOne(Cnds.of(Foo.class).eq(Foo::setId, id)));
    // 条件查询指定列
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    final Fields<Foo> queryColumn = Fields.of(Foo.class).include(Foo::getName).include(Foo::getId);
    assertNotNull(service.getOne(cnds, queryColumn));
    // 条件查询排序
    assertNotNull(service.getOne(Cnds.of(Foo.class).limit(1).orderBy(Foo::getId, Direction.DESC)));
    // 返回任意对象
    assertNotNull(
        service.getOne(
            Cnds.of(Foo.class).eq(Foo::setId, id),
            Fields.of(Foo.class).include(Foo::getId),
            IdNameResponse.class));
    // 自定义sql
    assertNotNull(service.fetchOneBySql("select id,name from foo limit 1", IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表")
  public void testList() {
    // 通过id列表查询
    assertNotNull(service.list(Arrays.asList(id, 2L, 3L)));
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    // 条件查询
    assertNotNull(service.list(cnds));
    // 查询指定列
    final Fields<Foo> queryColumn = Fields.of(Foo.class).include(Foo::getName).include(Foo::getId);
    assertNotNull(service.list(cnds, queryColumn, IdNameResponse.class));
    // 查询指定页
    cnds.limit(1, 10).orderBy(Foo::getId, Direction.DESC);
    assertNotNull(service.list(cnds));
    // 指定返回对象
    assertNotNull(service.list(cnds, IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页")
  public void testPage() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L).limit(1, 10).orderBy(Foo::getName);
    assertNotNull(service.page(cnds));
    // 指定列
    assertNotNull(
        service.page(cnds, Fields.of(Foo.class).include(Foo::getId).include(Foo::getName)));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate() {
    foo.setId(null);
    assertTrue(service.create(foo) > 0);
    // 保存指定字段
    final Fields<Foo> fields =
        Fields.of(Foo.class).include(Foo::getAge, Foo::isString, Foo::isNumber);
    // 排除指定字段
    // fields.exclude(Foo::getAge)
    assertTrue(service.create(foo, fields) > 0);
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch() {
    foo.setId(null);
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service
            .createBatch(
                list,
                Fields.of(Foo.class)
                    .include(
                        Foo::getName,
                        Foo::getTextString,
                        Foo::getBigText,
                        Foo::getType,
                        Foo::getColumn))
            .isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate() {
    assertEquals(service.update(foo), 1);
    foo.setName("<" + LocalDateTime.now() + ">");
    // 指定属性
    assertEquals(service.update(foo, Fields.of(Foo.class).include(Foo::getName)), 1);
    // 条件更新
    assertEquals(service.update(foo, Cnds.of(Foo.class).eq(Foo::setId, id)), 1);
    // 条件更新指定字段
    assertEquals(
        service.update(
            foo, //
            Fields.of(Foo.class).include(Foo::getName),
            Cnds.of(Foo.class).eq(Foo::setId, id)),
        1);
  }

  @Test
  @DisplayName("批量更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("test" + i * i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    // 可选指定更新字段
    assertFalse(service.updateBatch(list, Fields.of(Foo.class).include(Foo::getName)).isPresent());
  }

  @Test
  @Order(20)
  @DisplayName("删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete() {
    // 通过id删除
    assertEquals(service.delete(id), 1);
    // 通过id列表删除
    assertTrue(service.delete(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
    // 条件删除
    assertEquals(service.delete(Cnds.of(Foo.class).eq(Foo::setId, id)), 1);
  }

  @Test
  @Order(21)
  @DisplayName("域对象Domain")
  @Transactional(rollbackFor = Exception.class)
  public void testDomain() {
    // 需要对象继承Domain: public class Foo extends Domain<Foo, Long>
    foo.setId(null);
    assertTrue(foo.create() > 0);
    foo.setId(id);
    assertTrue(foo.update(Fields.of(Foo.class).include(Foo::getName)) > 0);
    assertTrue(foo.delete(Cnds.of(Foo.class).eq(Foo::setId, id)) > 0);
  }

  /**
   * @author ramer
   * @since 2020/8/5
   */
  @Getter
  @Setter
  public static class IdNameResponse {
    private Long id;
    private String name;
  }
}
