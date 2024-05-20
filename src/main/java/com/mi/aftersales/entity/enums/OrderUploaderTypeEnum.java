package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum OrderUploaderTypeEnum implements IEnum<Integer> {
    // 工单类型
    CLIENT(1, "客户"),
    ENGINEER(2, "工程师"),
    ;
    private int value;
    private String desc;

    OrderUploaderTypeEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
