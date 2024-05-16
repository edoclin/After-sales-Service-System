package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum ApiMethodEnum implements IEnum<Integer> {
    // 工单类型
    POST(1, "POST方法"),
    PUT(2, "PUT方法"),
    GET(3, "GET方法"),
    DELETE(4, "DELETE方法"),

    ;
    private int value;
    private String desc;

    ApiMethodEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
