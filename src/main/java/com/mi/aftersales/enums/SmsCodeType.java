package com.mi.aftersales.enums;

/**
 * @description: 发送验证码类型
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 22:59
 **/
public enum SmsCodeType {
    // 验证码类型
    BIND_LOGIN("绑定登录账户"),
    ORDER_NOTIFY("工单状态提醒");

    private String desc;

    SmsCodeType(String desc) {
        this.desc = desc;
    }
}
