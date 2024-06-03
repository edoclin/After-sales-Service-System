package com.mi.aftersales.enums.entity;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description: MyBatisPlus login type枚举填充
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum LoginTypeEnum implements IEnum<Integer> {
    // 1
    CLIENT(1, "客户"),
    // 2
    EMPLOYEE(2, "员工"),

    ;
    private int value;
    private String desc;

    LoginTypeEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
