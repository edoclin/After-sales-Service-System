package com.mi.aftersales.enums.entity;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum SmsResultEnum implements IEnum<Integer> {
    // 短信发送结果
    SUCCESS(1, "发送成功"),
    FAIL(2, "发送失败"),

    ;
    private int value;
    private String desc;

    SmsResultEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
