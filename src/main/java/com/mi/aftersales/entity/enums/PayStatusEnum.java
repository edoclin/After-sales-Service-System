package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum PayStatusEnum implements IEnum<Integer> {
    // 工单类型
    WAITING(1, "待支付"),
    PAID(2, "已支付"),
    TIMEOUT(3, "已过期"),

    ;
    private int value;
    private String desc;

    PayStatusEnum(int value, String desc) {
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
