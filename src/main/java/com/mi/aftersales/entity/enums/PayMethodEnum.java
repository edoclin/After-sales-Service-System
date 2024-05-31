package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum PayMethodEnum implements IEnum<Integer> {
    // 工单类型
    ALIPAY(1, "支付宝支付"),
    WEIXIN(2, "微信支付"),

    ;
    private int value;
    private String desc;

    PayMethodEnum(int value, String desc) {
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
