package com.mi.aftersales.enums.entity;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description: MyBatisPlus login source枚举填充
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum LoginOAuthSourceEnum implements IEnum<Integer> {
    // 1
    GITHUB(1, "GITHUB登录"),
    // 2
    MI(2, "小米登录"),

    ;
    private int value;
    private String desc;

    LoginOAuthSourceEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
