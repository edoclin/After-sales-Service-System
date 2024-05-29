package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.Order;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 工单 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface IOrderService extends IService<Order> {

    String NAMESPACE_4_PENDING_ORDER = "order:pending:";
    String CREATED = "CREATED";
    String WAITING = "WAITING";
    String ACCEPTED = "ACCEPTED";
    String CHECKING = "CHECKING";
    String FEE_CONFIRMING = "FEE_CONFIRMING";
    String FEE_CONFIRMED = "FEE_CONFIRMED";
    String MATERIAL_APPLYING = "MATERIAL_APPLYING";
    String MATERIAL_DISTRIBUTING = "MATERIAL_DISTRIBUTING";
    String REPAIRING = "REPAIRING";
    String RE_CHECKING = "RE_CHECKING";
    String TO_BE_PAID = "TO_BE_PAID";
    String PAID = "PAID";
    String RETURNING = "RETURNING";
    String CLOSED = "CLOSED";
    String STATE_MACHINE_HEADER_ORDER_NAME = "order-id";
    String CLIENT_CHOICE = "client-choice";
    String ENGINEER_CHOICE = "engineer-choice";
}
