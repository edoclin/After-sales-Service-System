package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum OrderStatusEnum implements IEnum<Integer> {
    // 工单状态定义
    CREATED(0, "已创建"),
    WAITING(1, "工单等待工程师处理"),
    ACCEPTED(2, "工单已被工程师处理"),
    CHECKING(3, "工程师检测中"),
    FEE_CONFIRMING(4, "等待用户确认计费"),
    MATERIAL_APPLYING(5, "物料申请中"),
    MATERIAL_DISTRIBUTING(6, "物料派发中"),
    REPAIRING(7, "工程师维修中"),
    RE_CHECKING(8, "工程师复检中"),
    TO_BE_PAID(9, "等待客户支付"),
    PAID(10, "客户已支付"),
    RETURNING(11, "物件返还给用户中"),
    CLOSED(12, "工单关闭"),

    ;
    private int value;
    private String desc;

    OrderStatusEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
