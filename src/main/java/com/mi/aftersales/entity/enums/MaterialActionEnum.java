package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum MaterialActionEnum implements IEnum<Integer> {
    // 库存操作
    STOCK_IN(1, "入库"),
    STOCK_OUT(2, "出库"),

    ;
    private int value;
    private String desc;

    MaterialActionEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
