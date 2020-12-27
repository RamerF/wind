package io.github.ramerf.wind.core.entity.response;

import lombok.ToString;

/**
 * 返回结果码.
 *
 * @since 2020.12.22
 * @author Tang Xiaofeng
 */
@ToString
public class ResultCode {
  // 凭证
  public static final ResultCode API_WRONG_PARAM = of(100, "参数错误");
  public static final ResultCode API_CERT_NOT_EXIST = of(101, "未知凭证");
  public static final ResultCode API_CERT_WRONG_FORMAT = of(102, "参数签名有误");
  public static final ResultCode API_CERT_SECRET_EXPIRE = of(103, "凭证已过期");
  public static final ResultCode API_CERT_SECRET_EMPTY = of(104, "凭证不能为空");
  public static final ResultCode API_CERT_DISABLED = of(105, "凭证已被禁用");
  public static final ResultCode API_CERT_UNKNOWN_ERROR = of(106, "其他原因");
  // 公共信息
  public static final ResultCode SUCCESS = of(0, "操作成功");
  public static final ResultCode ERROR = of(1, "系统繁忙,请稍后再试 !");
  public static final ResultCode API_UNAUTHORIZED = of(2, "未认证");
  public static final ResultCode API_FORBIDDEN = of(3, "无访问权限");
  public static final ResultCode API_NOT_IMPLEMENT = of(4, "方法未实现");
  public static final ResultCode API_SERVICE_NOT_AVAILABLE = of(5, "服务不可用");
  public static final ResultCode API_NOT_FOUND = of(6, "资源不可用");
  public static final ResultCode ERROR_DATA_ACCESS = of(7, "%s");
  // 操作失败提示
  public static final ResultCode API_FAIL_EXEC = of(1201, "操作失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_CREATE = of(1201, "添加失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_CREATE_EXIST = of(1202, "添加失败,数据已存在");
  public static final ResultCode API_FAIL_EXEC_UPDATE = of(1203, "更新失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_UPDATE_NOT_EXIST = of(1204, "更新失败,记录不存在");
  public static final ResultCode API_FAIL_EXEC_DELETE = of(1205, "删除失败");
  public static final ResultCode API_FAIL_EXEC_DELETE_BATCH = of(1206, "批量删除失败,可能数据已被删除");
  public static final ResultCode API_FAIL_DELETE_NO_CONDITION = of(1207, "删除失败,条件不能为空");
  /// 参数
  public static final ResultCode API_NOT_ALLOWED = of(1301, "操作不允许:[%s]");
  public static final ResultCode API_PARAM_WRONG_FORMAT = of(1302, "参数格式不正确:[%s]");
  public static final ResultCode API_PARAM_WRONG_VALUE = of(1303, "参数值不正确:[%s]");
  public static final ResultCode API_PARAM_CAN_NOT_BLANK = of(1304, "参数值不能为空:[%s]");
  public static final ResultCode API_PARAM_NOT_PRESENT = of(1305, "参数未传递:[%s]");
  public static final ResultCode API_PARAM_INVALID = of(1306, "参数值无效:[%s]");
  public static final ResultCode API_PARAM_BIND_ERROR = of(1307, "参数值有误: \n");
  public static final ResultCode API_DATA_EXIST = of(1308, "数据已存在:[%s]");
  public static final ResultCode API_DATA_NOT_EXIST = of(1309, "数据不存在:[%s]");
  public static final ResultCode API_CONTENT_TYPE_NOT_SUPPORT = of(1310, "请求ContentType不支持:[%s]");
  public static final ResultCode API_METHOD_NOT_SUPPORT = of(1311, "请求方式不支持:[%s]");
  public static final ResultCode API_TOO_MANY_RESULTS = of(1312, "返回结果过多");
  public static final ResultCode API_UPDATE_ID_NOT_EMPTY = of(1313, "id 不能为空");

  protected int code;
  protected String desc;

  public boolean isSuccess() {
    return code == SUCCESS.code;
  }

  protected ResultCode(final int code, final String desc) {
    this.code = code;
    this.desc = desc;
  }

  public static ResultCode of(final int code, final String desc) {
    return new ResultCode(code, desc);
  }

  public int code() {
    return code;
  }

  public String desc() {
    return desc;
  }

  public ResultCode desc(final String param) {
    return this.desc.contains("%s")
        ? new ResultCode(this.code, String.format(desc, param))
        : new ResultCode(this.code, String.format(this.desc.concat(":[%s]"), param));
  }
}
