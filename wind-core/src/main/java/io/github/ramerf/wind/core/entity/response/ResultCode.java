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

  // 临时修复,下个版本删除
  public static final ResultCode MA_HAS_BIND = of("E1400", "小程序已绑定其他企业");
  public static final ResultCode MA_ERROR_BIND = of("E1401", "小程序绑定企业失败");
  public static final ResultCode MA_HAS_BIND_OPEN = of("E1426", "小程序已绑定其他三方平台,请解绑后重试");
  public static final ResultCode MA_ERROR_GET_AUTHOR_INFO = of("E1402", "无法获取授权人帐号信息");
  public static final ResultCode MA_AUTH_CODE_EXPIRE = of("E1403", "授权已过期,请重新授权");
  public static final ResultCode MA_ERROR_GET_ACCESS_TOKEN = of("E1404", "无法获取微信码");
  public static final ResultCode MA_ERROR_UNBIND_COMPANY = of("E1405", "当前企业未绑定小程序");
  public static final ResultCode MA_ERROR_UNBIND_MA = of("E1406", "当前小程序未绑定企业");
  public static final ResultCode MA_ERROR_GET_TOKEN = of("E1407", "无法获取ACCESS_TOKEN");
  public static final ResultCode MA_ERROR_GET_CATEGORY = of("E1408", "无法获取订阅消息设置的类目");
  public static final ResultCode MA_ERROR_GET_PUB_TEMPLATE_TITLES = of("E1409", "无法获取订阅消息模板库标题列表");
  public static final ResultCode MA_ERROR_GET_PUB_TEMPLATE_KEYWORDS =
      of("E1410", "无法获取订阅消息模板库中某个模板标题下关键词库");
  public static final ResultCode MA_ERROR_GET_TEMPLATE = of("E1411", "无法获取订阅消息模板列表");
  public static final ResultCode MA_ERROR_TEMPLATE_RANGE_KEYWORDS = of("E1412", "2-5个关键词组合");
  public static final ResultCode MA_ERROR_TEMPLATE_RANGE_SCENE_DESC = of("E1413", "服务场景描述，15个字以内");
  public static final ResultCode MA_ERROR_ADD_TEMPLATE = of("E1414", "无法添加到个人模板库");
  public static final ResultCode MA_ERROR_DELETE_TEMPLATE = of("E1415", "无法删除个人模板");
  public static final ResultCode MA_ERROR_SEND_TEMPLATE = of("E1416", "无法发送模板消息");
  public static final ResultCode MA_ERROR_GET_VERIFY_RESULT = of("E1417", "无法获取审核结果");
  public static final ResultCode MA_ERROR_GET_PAGE_LIST = of("E1418", "无法获取页面列表");
  public static final ResultCode MA_ERROR_NOT_COMMIT_AUDIT = of("E1419", "未提交审核");
  public static final ResultCode MA_ERROR_GET_CODE_TEMPLATE_LIST = of("E1420", "无法获取模板库列表");
  public static final ResultCode MA_MISSING_FUNC_INFO =
      of("E1421", "请将所有权限授予[%s]<br/>如果小程序已绑定其它三方平台,请先解绑");
  public static final ResultCode MA_ERROR_GET_QRCODE = of("E1422", "无法获取小程序二维码");
  public static final ResultCode MA_NOT_EXIST_CODE_TEMPLATE = of("E1423", "没有可用的代码模板");
  public static final ResultCode MA_NOT_ALLOW_BIND_PERSONAL = of("E1424", "不允许绑定个人小程序");
  public static final ResultCode MA_ERROR_UPLOAD_TEMP_MEDIA = of("E1425", "上传临时素材失败");

  public static boolean isMaError(final ResultCode resultCode) {
    final int code = Integer.parseInt(resultCode.getCode().substring(1));
    return code >= 1400 && code <= 1499;
  }
  // 小程序托管

  // COMPANY  E01000-E0299
  // 登录与注册

  public static final ResultCode COMPANY_USER_NOT_EXIST = of("E01001", "用户不存在");
  public static final ResultCode COMPANY_FAIL_EXEC_COMPANY_SWITCH = of("E01002", "切换企业失败");
  public static final ResultCode COMPANY_LOGIN_TIMEOUT = of("E01003", "登录超时,请重新登录");
  public static final ResultCode COMPANY_CODE_INVALID = of("E01004", "验证码已失效，请重新发送验证码");
  public static final ResultCode COMPANY_CODE_ERROR = of("E01005", "验证码有误");
  public static final ResultCode COMPANY_PARAM_TYPE_ERROR = of("E01006", "type参数有误");
  public static final ResultCode COMPANY_PASSWORD_ERROR = of("E01007", "密码错误");
  public static final ResultCode COMPANY_LOGIN_KEY_ILLEGAL = of("E01008", "登录key非法");
  public static final ResultCode COMPANY_LOGIN_KEY_TIMEOUT = of("E01009", "登录已超时，请重新登录");
  public static final ResultCode COMPANY_NOT_EXIST = of("E01010", "当前公司不存在");
  public static final ResultCode COMPANY_EMPLOYEE_NOT_EXIST = of("E01011", "当前公司员工不存在");
  public static final ResultCode COMPANY_OUTSTRIP_NUMBER = of("E01012", "申请与已加入企业总和不能超过5个");
  public static final ResultCode COMPANY_APPLY_COMPANY_NUMBER = of("E01013", "最多同时申请3家企业");
  // 架构管理
  public static final ResultCode COMPANY_TEL_CAN_NOT_BLANK = of("E01014", "手机号不能为空");
  public static final ResultCode COMPANY_NOT_ALLOWED_MODIFY_BOSS = of("E01015", "不允许修改BOSS职位");
  public static final ResultCode COMPANY_NOT_ALLOWED_MODIFY_POSITION_BOSS =
      of("E01016", "不允许设置为BOSS职位");
  public static final ResultCode COMPANY_NOT_ALLOWED_DELETE_BOSS = of("E01017", "不允许删除BOSS职位");
  public static final ResultCode COMPANY_POSITION_EXIST = of("E01018", "职位已存在");
  public static final ResultCode COMPANY_POSITION_NOT_EXIST = of("E01019", "职位不存在");
  public static final ResultCode COMPANY_PARAM_ROLE_ERROR = of("E01020", "role参数请求错误");
  public static final ResultCode COMPANY_POSITION_RELATE_EMPLOYEE =
      of("E01020", "该职位下有员工，不允许删除，请移出员工后再删除");
  public static final ResultCode COMPANY_DEPARTMENT_EXIST = of("E01021", "部门已存在");
  public static final ResultCode COMPANY_STORE_EXIST = of("E01022", "门店已存在");
  public static final ResultCode COMPANY_STORE_NOT_EXIST = of("E01023", "没有门店,请先创建门店哦");
  public static final ResultCode COMPANY_STORE_RELATE_EMPLOYEE =
      of("E01024", "该门店下有员工,不允许删除,请移出员工后再删除");
  public static final ResultCode COMPANY_STORE_TAG_EXIST = of("E01025", "门店标签已存在");
  public static final ResultCode COMPANY_STORE_TAG_NOT_EXIST = of("E01026", "门店标签不存在");
  public static final ResultCode COMPANY_EMPLOYEE_EXISTED = of("E01027", "当前员工已存在");
  public static final ResultCode COMPANY_DEPARTMENT_RELATE_EMPLOYEE =
      of("E01028", "该部门下有员工，不允许删除，请移出员工后再删除");
  public static final ResultCode COMPANY_STORE_TAG_RELATE_STORE = of("E01029", "已有门店关联了此标签，不能删除");
  public static final ResultCode COMPANY_EXIST = of("E01030", "您已创建企业，可直接登录");
  public static final ResultCode COMPANY_EMPLOYEE_NOT_DELETE_FOR_ROLE = of("E01032", "当前职位不允许删除员工");
  public static final ResultCode COMPANY_EMPLOYEE_UNREGISTERED = of("E01033", "员工未注册");
  public static final ResultCode COMPANY_TEL_SHOULD_BE_ELEVEN_DIGITS = of("E01034", "电话号码应该是11位");
  public static final ResultCode COMPANY_TEL_IS_INVALID = of("E01035", "无效的电话号码");
  public static final ResultCode COMPANY_TEL_ALREADY_EXISTS = of("E01036", "该手机号已存在");
  public static final ResultCode COMPANY_EMPLOYEE_SELECT_EMPLOYEE_REMOVED =
      of("E01037", "请选择需要移除的员工");
  public static final ResultCode COMPANY_EMPLOYEE_HAS_RESIGN = of("E01037", "该员工已经离职");
  public static final ResultCode COMPANY_USERNAME_CONNOT_EMPTY = of("E01038", "用户名不能为空");
  public static final ResultCode COMPANY_NAME_CONNOT_EMPTY = of("E01039", "企业名不能为空");
  public static final ResultCode COMPANY_TEL_ALREADY_REGIST = of("E01040", "手机号已被注册");

  // JOB E01060-E01160
  public static final ResultCode COMPANY_JOB_NAME_MAX_LENGTH_50 = of("E01060", "职位名称不能超过50个字符");
  public static final ResultCode COMPANY_JOB_NAME_NOT_NULL = of("E01061", "职位名称不能为空");
  public static final ResultCode COMPANY_JOB_ROLE_CANNOT_SUPER_ADMIN = of("E01062", "职位角色不能为超级管理员");

  // DEPARTMENT E01161-E01260
  public static final ResultCode COMPANY_DEPARTMENT_NAME_MAX_LENGTH_50 =
      of("E01161", "部门名称不能超过50个字符");
  public static final ResultCode COMPANY_DEPARTMENT_NAME_NOT_NULL = of("E01162", "部门名称不能为空");

  // EMPLOYEE E01261-E01360
  public static final ResultCode COMPANY_EMPLOYEE_JOB_NOT_EXIST_REFUSE_GENERATE_CARD =
      of("E01261", "您还没有岗位信息,不能生成名片.");
  public static final ResultCode COMPANY_EMPLOYEE_PLEASE_COMPLETE_CARD_INFO =
      of("E01262", "请先去[我的名片]完善名片中的[形象照]和[个人介绍]信息");
  public static final ResultCode COMPANY_EMPLOYEE_PLEASE_COMPLETE_CARD_PERSONAL_PROFILE_INFO =
      of("E01263", "请先去[我的名片]完善名片中的[个人介绍]信息");
  public static final ResultCode COMPANY_ADMIN_CANNOT_IN_STORE = of("E01264", "管理员不允许关联门店");
  public static final ResultCode COMPANY_EMPLOYEE_DOES_NOT_EXIST = of("E01265", "该导购不存在");
  public static final ResultCode COMPANY_LOGIN_SASS_ROLE_ERROR = of("E01266", "非管理员权限不允许登录");
  public static final ResultCode COMPANY_TOKEN_ERROR = of("E01267", "token无效");
  public static final ResultCode COMPANY_EMPLOYEE_QUIT = of("E01268", "您已从当前企业离职,不能再进入该企业");
  public static final ResultCode COMPANY_PWD_EMPTY = of("E01269", "您还未设置初始密码,请使用验证码登录");
  public static final ResultCode COMPANY_PLEASE_INPUT_PWD = of("E01270", "请输入密码");
  public static final ResultCode COMPANY_PLEASE_INPUT_CODE = of("E01271", "请输入验证码");

  // knowledge  E0200-E0299
  // 知识库
  public static final ResultCode KNOWLEDGE_NAME_DUPLICATE = of("E02001", "操作失败,名称重复");
  public static final ResultCode KNOWLEDGE_TYPE_MISMATCH = of("E02002", "添加失败,类型不匹配");
  public static final ResultCode KNOWLEDGE_CLASSIFICATION_EXIST_FILE =
      of("E02003", "该知识库分类下有文件,不能执行删除操作");
  public static final ResultCode KNOWLEDGE_FILE_FORMAT_ERROR = of("E02004", "仅支持ppt类型文件");
  public static final ResultCode KNOWLEDGE_CASE_NOT_EXIST = of("E02005", "案例库不存在或已被删除");
  public static final ResultCode KNOWLEDGE_BROCHURE_NOT_EXIST = of("E02006", "宣传册不存在或已被删除");
  public static final ResultCode KNOWLEDGE_FILE_NOT_EXIST = of("E02007", "文件不能为空");
  public static final ResultCode KNOWLEDGE_FILENAME_NOT_EXIST = of("E02008", "文件名不能为空");
  public static final ResultCode KNOWLEDGE_CATEGORY_NOT_EXIST = of("E02009", "知识库分类不存在");
  public static final ResultCode KNOWLEDGE_BROCHURE_CATEGORY_NOT_EXIST = of("E02010", "宣传册分类不存在");
  public static final ResultCode KNOWLEDGE_VIDEO_CATEGORY_NOT_EXIST = of("E02011", "视频库分类不存在");
  public static final ResultCode KNOWLEDGE_VIDEO__NOT_EXIST = of("E02012", "视频不存在");
  public static final ResultCode KNOWLEDGE_TALKING__NOT_EXIST = of("E02013", "话术不存在");
  public static final ResultCode KNOWLEDGE_CATEGORY_NAME_NOT_EMPTY = of("E02014", "知识库分类名称不能为空");

  // H5
  public static final ResultCode H5_TYPE_MISMATCH = of("E03001", "操作失败,类型不匹配");
  public static final ResultCode H5_IMAGE_ILLEGAL = of("E03002", "敏感图片,上传失败");
  public static final ResultCode H5_TEXT_ILLEGAL = of("E03003", "文字包含敏感信息,请重新编辑");
  public static final ResultCode H5_PRODUCT_NOT_EXIST = of("E03004", "该商品不存在");
  public static final ResultCode H5_POSTER_CUSTOMER_SCENE_DOES_NOT_EXIST = of("E03005", "顾客场景不存在");
  public static final ResultCode H5_POSTER_GENERATE_PRODUCT_POSTER_FAILED =
      of("E03006", "生成商品海报失败");
  public static final ResultCode H5_POSTER_GENERATE_PERFORMANCE_DAILY_FAILED =
      of("E03007", "业绩日报生成失败");
  public static final ResultCode H5_POSTER_GENERATE_EMPLOYEE_CARD_POSTER_FAILED =
      of("E03008", "生成名片海报失败");
  public static final ResultCode H5_EMPLOYEE_ARTICLE_NOT_EXIST = of("E03009", "导购文章不存在或已被删除");
  public static final ResultCode H5_ARTICLE_NOT_EXIST = of("E03010", "文章不存在或已被删除");
  public static final ResultCode H5_POSTER_PLEASE_SELECT_SIX_PRODUCTS = of("E03011", "请选择6个商品");
  public static final ResultCode H5_BUYSHOW_SELECT_NINE_PRODUCTS = of("E03012", "最多只能选择9个商品");
  public static final ResultCode H5_BUYSHOW_PRODUCT_NOT_EXIST = of("E03013", "该商品不存在,请重新选择商品");
  public static final ResultCode H5_BUYSHOW_DETAIL_NOT_EMPTY = of("E03014", "买家秀详情不能为空");
  public static final ResultCode H5_BUYSHOW_NOT_EXIST = of("E03015", "买家秀不存在或已被删除");
  public static final ResultCode H5_BUYSHOW_PICTURE_SIZE = of("E03016", "最多只能添加9张图");
  public static final ResultCode H5_EMPLOYEE_BROCHURE_NOT_EXIST = of("E03017", "导购宣传册不存在或已被删除");
  public static final ResultCode H5_BROCHURE_NOT_EXIST = of("E03018", "宣传册不存在或已被删除");
  public static final ResultCode H5_POSTER_CUSTOMER_DOES_NOT_EXIST = of("E03019", "该顾客不存在");
  public static final ResultCode H5_SHARE_NOT_EXIST = of("E03020", "该分享不存在或已被删除");
  public static final ResultCode H5_CASE_NOT_EXIST = of("E03021", "案例库不存在或已被删除");
  public static final ResultCode H5_EMPLOYEE_CASE_NOT_EXIST = of("E03022", "导购案例库不存在或已被删除");
  public static final ResultCode H5_ARTICLE_NOT_BELONG_TO_EMPLOYEE = of("E03023", "文章不存在或不属于导购");
  public static final ResultCode H5_COMPANY_ARTICLE_NOT_EXIST = of("E03024", "企业文章不存在");
  public static final ResultCode H5_ARTICLE_URL_NOT_EXIST = of("E03025", "请输入文章链接");
  public static final ResultCode H5_MA_ERROR_AUTHORIZATION_COMPANY = of("E03026", "当前企业未授权微信小程序");
  public static final ResultCode H5_BUYSHOW_TYPE_ERROR = of("E03027", "买家秀类型不正确");

  // customer E0500-E0599
  public static final ResultCode API_NOT_EXISTS_MINIPROGRAM = of("E0500", "企业小程序不存在");
  public static final ResultCode API_FAIL_EXEC_OPEN_OR_CLOSE_VIP = of("E0501", "开启/关闭会员服务失败");
  public static final ResultCode API_FAIL_EXEC_CALLER_UP_CUSTOMER = of("E0502", "访客升级为客户失败");
  public static final ResultCode API_FAIL_EXEC_MAX_VIP_LEVEL = of("E0503", "该公司下还没有会员等级");
  public static final ResultCode API_FAIL_EXEC_CUSTOMER_TAG_REPEAT = of("E0504", "客户标签已存在，请重新输入");
  public static final ResultCode API_FAIL_EXEC_VIP_LEVEL_REPEAT = of("E0505", "会员等级已存在，请重新输入");
  public static final ResultCode API_FAIL_EXEC_VIP_NAME_REPEAT = of("E0506", "会员名称已存在，请重新输入");
  public static final ResultCode API_FAIL_EXEC_VIP_CONFIG_PRESENT = of("E0507", "vip配置表不存在");
  public static final ResultCode CUSTOMER_VIP_CLOSE_ERR = of("E0510", "该企业下会员等级已有会员，不能关闭");
  public static final ResultCode CUSTOMER_VIP_CREATE_ERR = of("E0511", "会员等级添加失败");
  public static final ResultCode CUSTOMER_VIP_MEMBER_EXIST_ERR = of("E0514", "该等级下已有会员，不能删除");
  public static final ResultCode CUSTOMER_SHARE_ERR = of("E0518", "客户分享次数更新失败");
  public static final ResultCode CUSTOMER_SCENE_CREATE_ERROR = of("E0528", "创建场景值失败");
  public static final ResultCode CUSTOMER_UPDATE_VIP_ERR = of("E0529", "会员等级更新失败");
  public static final ResultCode CUSTOMER_VIP_MAX_ERR = of("E0530", "会员等级不能超过6级");
  public static final ResultCode CUSTOMER_TAG_UPDATE_ERR = of("E0530", "编辑客户标签失败");
  public static final ResultCode CUSTOMER_SHARE_NO_EXIST_ERR = of("E0531", "该分享客户不存在");
  public static final ResultCode CUSTOMER_DEL_VIP_ERR = of("E0532", "会员等级删除失败");
  public static final ResultCode CUSTOMER_NO_EXIST = of("E0533", "客户不存在");


  /** order E0601-E0699 */
  public static final ResultCode ORDER_INVALID_ERR = of("E0601", "订单无效");

  public static final ResultCode ORDER_STATUS_INVALID_ERR = of("E0602", "订单状态异常");
  public static final ResultCode ORDER_NOT_MODIFY_JOIN_CUSTOMER_ERR = of("E0603", "已支付订单不能修改关联用户");
  public static final ResultCode ORDER_PRODUCT_PRICE_CHANGED_PLEASE_RESUBMIT_ERR =
      of("E0604", "商品[%s]的一口价发生了变化,请重新下单");
  public static final ResultCode ORDER_PRODUCT_REMOVED_PLEASE_RESUBMIT_ERR =
      of("E0605", "商品[%s]已被下架,请重新下单");
  public static final ResultCode ORDER_PRODUCT_NO_EXIST_PLEASE_RESUBMIT_ERR =
      of("E0606", "商品不存在,请重新下单");
  public static final ResultCode ORDER_PRODUCT_NO_STOCK_ERR = of("E0607", "商品库存不足");

  public static final ResultCode ORDER_CUSTOMER_CART_INVALID_ERR = of("E0611", "购物车异常");
  public static final ResultCode ORDER_PRODUCTS_NOT_LESS_THAN_ONE_ERR = of("E0612", "商品数量不能小于1");

  public static final ResultCode ORDER_PAY_CODE_ROLLBACK_ERR = of("E0621", "付款码支付撤单失败");
  public static final ResultCode ORDER_WX_PAY_WAIT_USER_PAY = of("E0622", "微信加密支付,等待用户输入密码...");
  public static final ResultCode ORDER_WX_PAY_SUCCESS = of("E0623", "微信免密支付成功");
  public static final ResultCode ORDER_WX_PAY_FAIL = of("E0624", "微信支付失败");

  public static final ResultCode ORDER_COMPANY_PAYMENT_TEMPLATE_INVALID = of("E0631", "企业支付模板无效");
  public static final ResultCode ORDER_COMPANY_PAYMENT_TEMPLATE_CONFIGURED =
      of("E0632", "企业已配置支付模板");
  public static final ResultCode ORDER_COMPANY_PRODUCT_NOT_CONFIGURED_ERR = of("E0633", "企业未配置该产品");
  public static final ResultCode ORDER_COMPANY_PAYMENT_TEMPLATE_NOT_CONFIGURED_ERR =
      of("E0634", "企业未配置支付模板");

  /** activity E0701-E0799 */
  public static final ResultCode ACTIVITY_INVALID_ERR = of("E0701", "活动无效");

  public static final ResultCode ACTIVITY_OVER_ERR = of("E0702", "活动已结束");
  public static final ResultCode ACTIVITY_IS_LIMITED_ERR = of("E0703", "已到达领取上线");

  public static final ResultCode ACTIVITY_COUPON_OVER_ERR = of("E0711", "优惠券已领取完");
  public static final ResultCode ACTIVITY_COUPON_INVALID_ERR = of("E0712", "优惠券无效");
  public static final ResultCode ACTIVITY_COUPON_USED_ERR = of("E0713", "优惠券已使用");
  public static final ResultCode ACTIVITY_COUPON_EXPIRED_ERR = of("E0714", "优惠券已过期");
  public static final ResultCode ACTIVITY_COUPON_LOCK_ERR = of("E0715", "优惠券已锁定");
  public static final ResultCode ACTIVITY_COUPON_NUMBER_LIMIT_ERR =
      of("E0716", "优惠券发放数量不能减少且不能超过10万张");
  public static final ResultCode ACTIVITY_COUPON_RELATION_EDIT_ERR = of("E0717", "关联券不能修改");
  public static final ResultCode ACTIVITY_COUPON_RELATION_DEL_ERR = of("E0718", "关联券不能删除");

  public static final ResultCode ACTIVITY_GIFT_OVER_ERR = of("E0721", "礼包已领取完");
  public static final ResultCode ACTIVITY_GIFT_INVALID_ERR = of("E0722", "礼包无效");
  public static final ResultCode ACTIVITY_GIFT_USED_ERR = of("E0723", "礼包已使用");
  public static final ResultCode ACTIVITY_GIFT_EXPIRED_ERR = of("E0724", "礼包已过期");

  /** page E0801-E0899 */
  public static final ResultCode PAGE_INVALID_ERR = of("E0801", "轻页面无效");

  public static final ResultCode PAGE_TITLE_EXIST_ERR = of("E0802", "轻页面名称已存在");
  public static final ResultCode PAGE_TYPE_EXIST_ERR = of("E0803", "轻页面类型已存在");
  public static final ResultCode PAGE_SYSTEM_INVALID_ERR = of("E0804", "非系统轻页面");
  public static final ResultCode PAGE_SYSTEM_NO_INIT_ERR = of("E0805", "系统轻页面未初始化");

  public static final ResultCode PAGE_CONFIG_INVALID_ERR = of("E0811", "轻页面配置无效");

  /** product E0901-E0999 */
  public static final ResultCode PRODUCT_NOT_EXIST_ERR = of("E0901", "商品不存在");

  /** 服务调用异常 */
  public static final ResultCode API_TRANSFER_ERR = of("E1401", "服务调用异常:[%s]");

  public static final ResultCode API_TRANSFER_ORDER_STORE_DETAIL_ERR = of("E1402", "订单查询门店详情异常");
  public static final ResultCode API_TRANSFER_ORDER_EMPLOYEE_DETAIL_ERR = of("E1403", "订单查询员工详情异常");
  public static final ResultCode API_TRANSFER_ORDER_CUSTOMER_DETAIL_ERR = of("E1404", "订单查询客户信息异常");
  public static final ResultCode API_TRANSFER_ORDER_CUSTOMER_VIP_DETAIL_ERR =
      of("E1405", "订单查询客户信息vip异常");
  public static final ResultCode API_TRANSFER_ORDER_REDUCE_INVENTORY_ERR = of("E1406", "订单减库存接口异常");

  public static final ResultCode API_TRANSFER_ORDER_PRODUCT_INFOS_ERR =
      of("E1407", "订单查询商品的基本信息异常");
  public static final ResultCode API_TRANSFER_ORDER_PAYMENT_TEMPLATE_BATCH_PUS_ERR =
      of("E1408", "订单支付模板批量投放异常");

  public static final ResultCode API_TRANSFER_ORDER_ACTIVITY_COUPON_SELECT_ERR =
      of("E1409", "计算优惠券优惠额度异常");

  public static final ResultCode API_TRANSFER_ORDER_RECOVER_INVENTORY_ERR =
      of("E1410", "订单还原库存接口异常");
  public static final ResultCode API_TRANSFER_ORDER_STORE_LIST_ERR = of("E1411", "订单查询门店列表异常");
  public static final ResultCode API_TRANSFER_PRODUCT_CUSTOMER_DETAIL_ERR =
      of("E1412", "商品分析查询客户信息异常");
  // 客户
  public static final ResultCode API_TRANSFER_CUSTOMER_COMPANY_LIST_STORE_ERR =
      of("E1413", "查询企业门店异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_COMPANY_LIST_EMPLOYEE_ERR =
      of("E1414", "查询企业员工异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_RECENT_ERR =
      of("E1415", "客户查询最近消费订单异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_CART_ERR = of("E1416", "查询加购客户列表异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_COMPANY_STORE_ERR =
      of("E1417", "查询门店客户列表异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_COMPANY_EMPLOYEE_ERR =
      of("E1418", "查询导购客户列表异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_LIST_ERR = of("E1419", "查询客户订单列表异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_FIRST_ORDER_ERR =
      of("E1420", "查询近7天首次成交客户异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_SEVEN_BUY_ERR =
      of("E1421", "查询近7天购买过客户异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_ORDER_DAY_ERR =
      of("E1422", "查询某一天下单的客户ID列表异常");
  public static final ResultCode API_TRANSFER_DATACENTER_CUSTOMER_ANALYSIS_ERR =
      of("E1423", "客户统计分析获取异常");
  public static final ResultCode API_TRANSFER_DATACENTER_MA_AUTHOR_INFO_ERR =
      of("E1424", "小程序列表获取异常");
  public static final ResultCode API_TRANSFER_PRODUCT_CONFIG_SERVICE_ERR =
      of("E1425", "商品详情查询公司配置服务异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_PRODUCT_SERVICE_ERR =
      of("E1426", "客户查询商品详情异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_COMPANY_LIST_ERR =
      of("E1427", "查询导购的客户ID列表异常");
  public static final ResultCode API_TRANSFER_DATACENTER_CUSTOMER_LIST_ERR =
      of("E1428", "查询导购的客户ID列表异常");
  public static final ResultCode API_TRANSFER_DATACENTER_CUSTOMER_SOURCE_ERR =
      of("E1429", "查询客户来源异常");
  public static final ResultCode API_TRANSFER_CUSTOMER_EMPLOYEE_ERR = of("E1430", "查询导购异常");
  public static final ResultCode API_TRANSFER_DATACENTER_COMPANY_SHOP_ERR =
      of("E1431", "判断员工是否为店长异常");

  // 素材模块
  public static final ResultCode MATERIAL_NO_PAY_QR_CODE = of("E1501", "请上传付款二维码");
  public static final ResultCode MATERIAL_UPLOAD_FILE_TO_OSS_FAILED = of("E1501", "文件上传到OSS失败");

  // 媒体模块
  public static final ResultCode MEDIA_VIDEO_FIRST_FRAME_FAILED = of("E1601", "截取视频第一帧图片失败");

  /** 服务调用异常 1431-1499 */
  public static final ResultCode API_TRANSFER_ORDER_ACTIVITY_COUPON_LOCK_ERR =
      of("E1441", "锁定优惠券异常");

  public static final ResultCode API_TRANSFER_ORDER_ACTIVITY_COUPON_UNLOCK_ERR =
      of("E1442", "解除锁定优惠券异常");
  public static final ResultCode API_TRANSFER_ORDER_ACTIVITY_COUPON_USE_ERR =
      of("E1443", "优惠券使用异常");

  public static final ResultCode API_TRANSFER_ACTIVITY_ORDER_COUPON_USER_ERR =
      of("E1445", "订单转化率获取异常");

  /** activity E0731-E0799 */
  public static final ResultCode ACTIVITY_GIFT_NO_END_ERR = of("E0731", "未结束礼包不可删除");

  public static final ResultCode ACTIVITY_GIFT_NO_COUPON_ERR = of("E0731", "礼包未关联优惠券");

  public static final ResultCode ACTIVITY_COUPON_NO_END_ERR = of("E0732", "未结束优惠券不可删除");
  public static final ResultCode API_TRANSFER_CONFIG_COMPANY_CITY_ERR =
      of("E1432", "config服务获取城市数据异常");
  public static final ResultCode H5_POSTER_GENERATE_APPLET_QR_CODE_PICTURE_FAILED =
      of("E1433", "生成二维码失败");
}
