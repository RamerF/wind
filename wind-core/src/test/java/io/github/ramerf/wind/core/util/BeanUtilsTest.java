package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import lombok.extern.slf4j.Slf4j;
import org.junit.*;

/**
 * BeanUtils Tester.
 *
 * @author RAMER
 * @since 12/29/2019
 * @version 1.0
 */
// @RunWith(SpringRunner.class)
// @RunWith(JUnit4.class)
// @SpringBootTest
@Slf4j
public class BeanUtilsTest {

  @Before
  public void before() throws Exception {}

  @After
  public void after() throws Exception {}

  @Test
  public void testMapToBean() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testGetNullPropertyNames() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testInitialClazz() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testInitialClassPath() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testGetClazz() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testGetPrivateFields() throws Exception {
    log.info(" BeanUtils.main : [{}]", BeanUtils.getPrivateFields(Ts.class, true));
  }

  @Test
  public void testGetPoJoClass() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testGetResponseClass() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testMethodToColumnMethod() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testMethodToColumnFunction() throws Exception {
    System.out.println("not implement yet");
  }

  @Test
  public void testMethodToColumnArr() throws Exception {
    System.out.println("not implement yet");
  }
}

class TP implements AbstractEntity {
  private Long tpid;
}

class Ts extends TP {
  private String tname;
  private Te1 te1;
}

class Te1 extends Te3 implements AbstractEntity {
  private String te1password;
  private Te2 te2;
}

class Te2 extends Te1 implements AbstractEntity {
  private String te2password;
}

class Te3 implements AbstractEntity {
  private String te3hah;
}
