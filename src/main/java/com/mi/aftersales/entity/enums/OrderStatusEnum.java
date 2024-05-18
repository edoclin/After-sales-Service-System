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
    FEE_CONFIRMED(5, "用户已完成确认计费"),
    MATERIAL_APPLYING(6, "物料申请中"),
    MATERIAL_DISTRIBUTING(7, "物料派发中"),
    REPAIRING(8, "工程师维修中"),
    RE_CHECKING(9, "工程师复检中"),
    TO_BE_PAID(10, "等待客户支付"),
    PAID(11, "客户已支付"),
    RETURNING(12, "物件返还给用户中"),
    CLOSED(13, "工单关闭"),

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
