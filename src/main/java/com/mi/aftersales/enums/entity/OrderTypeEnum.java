package com.mi.aftersales.enums.entity;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum OrderTypeEnum implements IEnum<Integer> {
    // 工单类型
    SEND_FOR(1, "寄修"),
    TO_SHOP(2, "送修"),

    ;
    private int value;
    private String desc;

    OrderTypeEnum(int value, String desc) {
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
