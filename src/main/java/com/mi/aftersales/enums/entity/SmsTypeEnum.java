package com.mi.aftersales.enums.entity;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum SmsTypeEnum implements IEnum<Integer> {
    // 短信类型
    LOGIN(1, "用户登录"),
    ORDER(2, "工单状态"),

    ;
    private int value;
    private String desc;

    SmsTypeEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }

    public String getDesc() {
        return desc;
    }
}
