package io.github.ramerf.wind.demo.entity.response;

import io.github.ramerf.wind.core.entity.response.ResultCode;
import lombok.ToString;

/**
 * 响应码.
 *
 * @since 2020.12.22
 * @author Tang Xiaofeng
 */
@ToString
public class ResCode extends ResultCode {
  public static final ResultCode FOO_SUCCESS_UPDATE = of(9100, "更新成功!");
  public static final ResultCode FOO_FAIL_UPDATE = of(9101, "哇哦 \uD83D\uDE05,更新失败了,要不要重新试下 ?");
  public static final ResultCode FOO_SUCCESS_DELETE = of(9110, "删除成功!");
  public static final ResultCode FOO_FAIL_DELETE = of(9111, "哇哦 \uD83D\uDE05,删除失败了,要不要重新试下 ?");

  protected ResCode(final int code, final String desc) {
    super(code, desc);
  }
}
