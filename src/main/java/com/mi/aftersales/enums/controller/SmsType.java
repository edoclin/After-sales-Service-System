package com.mi.aftersales.enums.controller;

import lombok.Getter;

/**
 * @description: 发送验证码类型
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 22:59
 **/
@Getter
public enum SmsType  {
    // 验证码类型
    LOGIN("登录账户", "login"),
    ORDER_NOTIFY("工单状态提醒", "order");

    private String desc;
    private String value;

    SmsType(String desc, String value) {
        this.desc = desc;
        this.value = value;
    }
}
