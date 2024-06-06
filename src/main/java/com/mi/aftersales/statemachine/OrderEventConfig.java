package com.mi.aftersales.statemachine;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderStatusFlowException;
import com.mi.aftersales.repository.IOrderRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.Message;
import org.springframework.statemachine.annotation.OnTransition;
import org.springframework.statemachine.annotation.WithStateMachine;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

import static com.mi.aftersales.common.StateMachineOrderStatus.*;


/**
 * @description: 订单状态机
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 16:56
 **/
@WithStateMachine(id = OrderStateMachineBuilder.MACHINE_ID)
@Slf4j
@Transactional
public class OrderEventConfig {
    @Resource
    private IOrderRepository iOrderRepository;

    /**
     * @description: 客户完成工单创建
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/
    @OnTransition(source = CREATED, target = WAITING)
    public boolean createOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        order.setOrderStatus(OrderStatusEnum.WAITING);
        return iOrderRepository.updateById(order);
    }

    /**
     * @description: 工程师接受处理工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/

    @OnTransition(source = WAITING, target = ACCEPTED)
    public boolean acceptOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        order.setOrderStatus(OrderStatusEnum.ACCEPTED);
        order.setEngineerLoginId(StpUtil.getLoginIdAsString());
        return iOrderRepository.updateById(order);
    }

    /**
     * @description: 工程师开始检测
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = ACCEPTED, target = CHECKING)
    public boolean checkingOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get("order-id"));
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderStatusFlowException();
        }
        order.setOrderStatus(OrderStatusEnum.CHECKING);
        return iOrderRepository.updateById(order);
    }

    /**
     * @description: 工程师完成计费，等待用户确认
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = CHECKING, target = FEE_CONFIRMING)
    public boolean confirmingFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.FEE_CONFIRMING);
    }

    /**
     * @description: 用户确认维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = FEE_CONFIRMING, target = FEE_CONFIRMED)
    public boolean confirmedFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.FEE_CONFIRMED);
    }

    /**
     * @description: 工程师申请物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = FEE_CONFIRMED, target = MATERIAL_APPLYING)
    public boolean applyingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.MATERIAL_APPLYING);
    }

    /**
     * @description: 库管分发物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = MATERIAL_APPLYING, target = MATERIAL_DISTRIBUTING)
    public boolean distributingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.MATERIAL_DISTRIBUTING);
    }

    /**
     * @description: 工程师开始维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = MATERIAL_DISTRIBUTING, target = REPAIRING)
    public boolean repairOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.REPAIRING);
    }

    /**
     * @description: 工程师开始复检
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = REPAIRING, target = RE_CHECKING)
    public boolean reCheckingOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RE_CHECKING);
    }

    /**
     * @description: 工程师完成复检，发起支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = RE_CHECKING, target = TO_BE_PAID)
    public boolean toBePaidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.TO_BE_PAID);
    }

    /**
     * @description: 客户完成支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = TO_BE_PAID, target = PAID)
    public boolean paidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.PAID);
    }

    /**
     * @description: 用户拒绝维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/20 21:50
     **/
    @OnTransition(source = FEE_CONFIRMING, target = RETURNING)
    public boolean rejectRepairTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RETURNING);
    }


    /**
     * @description: 用户完成支付，工程师返回物品
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = PAID, target = RETURNING)
    public boolean returningOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RETURNING);
    }

    /**
     * @description: 客户关闭工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = RETURNING, target = CLOSED)
    public boolean closedOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.CLOSED);
    }

    /**
     * @description: 客户完成费用确认，工程师开始维修（无需物料）
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = FEE_CONFIRMED, target = REPAIRING)
    public boolean startRepairOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.REPAIRING);
    }


    private boolean updateOrderStatus(Message<OrderStatusChangeEventEnum> message, OrderStatusEnum target) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get("order-id"));
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        order.setOrderStatus(target);
        return iOrderRepository.updateById(order);
    }
}