package com.mi.aftersales.common;


/**
 * @description: 工单状态
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 21:28
 **/
public class StateMachineOrderStatus {
    private StateMachineOrderStatus(){}

    public static final String CREATED = "CREATED";
    public static final String WAITING = "WAITING";
    public static final String ACCEPTED = "ACCEPTED";
    public static final String CHECKING = "CHECKING";
    public static final String FEE_CONFIRMING = "FEE_CONFIRMING";
    public static final String FEE_CONFIRMED = "FEE_CONFIRMED";
    public static final String MATERIAL_APPLYING = "MATERIAL_APPLYING";
    public static final String MATERIAL_DISTRIBUTING = "MATERIAL_DISTRIBUTING";
    public static final String REPAIRING = "REPAIRING";
    public static final String RE_CHECKING = "RE_CHECKING";
    public static final String TO_BE_PAID = "TO_BE_PAID";
    public static final String PAID = "PAID";
    public static final String RETURNING = "RETURNING";
    public static final String CLOSED = "CLOSED";
    public static final String STATE_MACHINE_HEADER_ORDER_NAME = "order-id";
    public static final String STATE_MACHINE_HEADER_CATEGORY_ID = "category-id";
    public static final String CLIENT_CHOICE = "client-choice";
    public static final String ENGINEER_CHOICE = "engineer-choice";
}
