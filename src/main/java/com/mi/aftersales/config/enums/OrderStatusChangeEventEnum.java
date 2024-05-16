package com.mi.aftersales.config.enums;


/**
 * @description: 状态触发事件
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 15:54
 **/
public enum OrderStatusChangeEventEnum {
    // 工程师接受工单 -> 2.工单已受理
    ENGINEER_COMPLETED_ACCEPT,
    // 工程师开始检测 -> 3.检测处理
    ENGINEER_START_CHECKING,
    // 工程师确认费用 -> 4.费用确认
    ENGINEER_COMPLETED_FEE_CONFIRM,
    // 工程师申请物料 -> 5.物料申请
    ENGINEER_APPLIED_MATERIAL,
    // 管理员发放物料 -> 6.物料派发
    MANAGER_DISTRIBUTED_MATERIAL,
    // 工程师收到物料 -> 7.维修
    ENGINEER_RECEIVED_MATERIAL,
    // 无需物料， 直接开始维修-> 7.维修
    ENGINEER_START_REPAIR,
    // 工程师开始复检 -> 8复检
    ENGINEER_START_RECHECK,
    // 工程师复检完成 -> 9.待支付
    ENGINEER_COMPLETED_RECHECK,
    // 客户完成支付 -> 10.已支付
    CLIENT_COMPLETED_PAY,
    // 工程师完成交付 -> 11.返还客户
    ENGINEER_COMPLETED_RETURN,
    // 用户关闭工单 -> 12.关闭工单
    CLIENT_CLOSED
}