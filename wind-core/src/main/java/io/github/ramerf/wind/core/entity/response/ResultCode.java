package io.github.ramerf.wind.core.entity.response;

import lombok.Data;

/**
 * 返回结果码,名称前缀为模块简拼.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/5
 */
@Data
@SuppressWarnings("all")
public class ResultCode {
  // 凭证
  public static final ResultCode API_WRONG_PARAM = of("E0100", "参数错误[code,data,signed]不能为空");
  public static final ResultCode API_CERT_NOT_EXIST = of("E0101", "未知凭证");
  public static final ResultCode API_CERT_WRONG_FORMAT = of("E0102", "参数签名有误");
  public static final ResultCode API_CERT_SECRET_EXPIRE = of("E0103", "凭证已过期");
  public static final ResultCode API_CERT_SECRET_EMPTY = of("E0104", "凭证不能为空");
  public static final ResultCode API_CERT_DISABLED = of("E0105", "凭证已被禁用");
  public static final ResultCode API_CERT_UNKNOWN_ERROR = of("E0106", "其他原因");
  // 公共信息
  public static final ResultCode SUCCESS = of("E0000", "操作成功");
  public static final ResultCode ERROR = of("E0001", "系统繁忙,请稍后再试 !");
  public static final ResultCode API_UNAUTHORIZED = of("E0002", "未认证");
  public static final ResultCode API_FORBIDDEN = of("E0003", "无访问权限");
  public static final ResultCode API_NOT_IMPLEMENT = of("E0004", "方法未实现");
  public static final ResultCode API_SERVICE_NOT_AVAILABLE = of("E0005", "服务不可用");
  public static final ResultCode API_NOT_FOUND = of("E0006", "资源不可用");
  public static final ResultCode ERROR_DATA_ACCESS = of("E0007", "%s");
  // 操作成功提示
  public static final ResultCode API_SUCCESS_EXEC_CREATE = of("E1100", "添加成功");
  public static final ResultCode API_SUCCESS_EXEC_UPDATE = of("E1101", "更新成功");
  public static final ResultCode API_SUCCESS_EXEC_DELETE = of("E1102", "删除成功");
  // 操作失败提示
  public static final ResultCode API_FAIL_EXEC = of("E1201", "操作失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_ADD = of("E1201", "添加失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_ADD_EXIST = of("E1202", "添加失败,数据已存在");
  public static final ResultCode API_FAIL_EXEC_UPDATE = of("E1203", "更新失败,数据格式有误");
  public static final ResultCode API_FAIL_EXEC_UPDATE_NOT_EXIST = of("E1204", "更新失败,记录不存在");
  public static final ResultCode API_FAIL_EXEC_DELETE = of("E1205", "删除失败");
  public static final ResultCode API_FAIL_EXEC_DELETE_BATCH = of("E1206", "批量删除失败,可能数据已被删除");
  public static final ResultCode API_FAIL_DELETE_NO_CONDITION = of("E1207", "删除失败,条件不能为空");
  /// 参数
  public static final ResultCode API_NOT_ALLOWED = of("E1301", "操作不允许:[%s]");
  public static final ResultCode API_PARAM_WRONG_FORMAT = of("E1302", "参数格式不正确:[%s]");
  public static final ResultCode API_PARAM_WRONG_VALUE = of("E1303", "参数值不正确:[%s]");
  public static final ResultCode API_PARAM_CAN_NOT_BLANK = of("E1304", "参数值不能为空:[%s]");
  public static final ResultCode API_PARAM_NOT_PRESENT = of("E1305", "参数未传递:[%s]");
  public static final ResultCode API_PARAM_INVALID = of("E1306", "参数值无效:[%s]");
  public static final ResultCode API_PARAM_BIND_ERROR = of("E1307", "参数值有误: \n");
  public static final ResultCode API_DATA_EXIST = of("E1308", "数据已存在:[%s]");
  public static final ResultCode API_DATA_NOT_EXIST = of("E1309", "数据不存在:[%s]");
  public static final ResultCode API_CONTENT_TYPE_NOT_SUPPORT =
      of("E1310", "请求ContentType不支持:[%s]");
  public static final ResultCode API_METHOD_NOT_SUPPORT = of("E1311", "请求方式不支持:[%s]");
  public static final ResultCode API_TOO_MANY_RESULTS = of("E1312", "返回结果过多");
  public static final ResultCode API_UPDATE_ID_NOT_EMPTY = of("E1313", "id 不能为空");

  protected String code;
  protected String desc;

  public boolean isSuccess() {
    return code.equals(SUCCESS.code);
  }

  protected ResultCode(final String code, final String desc) {
    this.code = code;
    this.desc = desc;
  }

  public static ResultCode of(final String code, final String desc) {
    return new ResultCode(code, desc);
  }

  public String code() {
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
