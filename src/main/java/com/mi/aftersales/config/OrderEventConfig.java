package com.mi.aftersales.config;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IOrderService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.annotation.OnTransition;
import org.springframework.statemachine.annotation.WithStateMachine;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;


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

    public static final String ROCKETMQ_TOPIC_4_ORDER_LOG = "order-log-topic";

    @Resource
    private IOrderService iOrderService;


    @Resource
    private RedisTemplate<String, String> redisTemplate;

    @Resource
    private RocketMQTemplate rocketmqTemplate;


    /**
     * @description: 客户完成工单创建
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/
    @OnTransition(source = IOrderService.CREATED, target = IOrderService.WAITING)
    public boolean createOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderService.getById((String) message.getHeaders().get("order-id"));

        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("工单状态转换：工单Id不合法！");
        }
        order.setOrderStatus(OrderStatusEnum.WAITING);

        OrderStatusLog orderStatusLog = new OrderStatusLog();

        orderStatusLog
                .setCreatedId(StpUtil.getLoginIdAsString())
                .setOrderId(order.getOrderId())
                .setOrderStatus(order.getOrderStatus())
                .setStatusDetail(CharSequenceUtil.format("工单创建成功，等待工程师处理！"));

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();

        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);

        if (Boolean.FALSE.equals(iOrderService.updateById(order))) {
            throw new ServerErrorException();
        }

        // 工程师可接单
        redisTemplate.opsForSet().add(IOrderService.NAMESPACE_4_PENDING_ORDER, order.getOrderId());
        return true;
    }

    /**
     * @description: 工程师接受处理工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/

    @OnTransition(source = IOrderService.WAITING, target = IOrderService.ACCEPTED)
    public boolean acceptOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderService.getById((String) message.getHeaders().get("order-id"));

        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("工单状态转换：工单Id不合法！");
        }

        order.setOrderStatus(OrderStatusEnum.ACCEPTED);
        order.setEngineerLoginId(StpUtil.getLoginIdAsString());

        OrderStatusLog orderStatusLog = new OrderStatusLog();

        orderStatusLog.setOrderId(order.getOrderId()).setOrderStatus(order.getOrderStatus()).setStatusDetail(CharSequenceUtil.format("工单已被受理，等待工程师（{}）处理！", StpUtil.getLoginIdAsString()));

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();

        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);

        if (Boolean.FALSE.equals(iOrderService.updateById(order))) {
            throw new ServerErrorException();
        }
        return true;
    }

    /**
     * @description: 工程师开始检测
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.ACCEPTED, target = IOrderService.CHECKING)
    public boolean checkingOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师完成计费，等待用户确认
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.CHECKING, target = IOrderService.FEE_CONFIRMING)
    public boolean confirmingFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师完成计费，等待用户确认
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.FEE_CONFIRMING, target = IOrderService.FEE_CONFIRMED)
    public boolean confirmedFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师申请物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.FEE_CONFIRMED, target = IOrderService.MATERIAL_APPLYING)
    public boolean applyingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 库管分发物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.MATERIAL_APPLYING, target = IOrderService.MATERIAL_DISTRIBUTING)
    public boolean distributingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师开始维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.MATERIAL_DISTRIBUTING, target = IOrderService.REPAIRING)
    public boolean repairOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师开始复检
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.REPAIRING, target = IOrderService.RE_CHECKING)
    public boolean reCheckingOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师完成复检，发起支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.RE_CHECKING, target = IOrderService.TO_BE_PAID)
    public boolean toBePaidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 客户完成支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.TO_BE_PAID, target = IOrderService.PAID)
    public boolean paidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 工程师返回物品
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.PAID, target = IOrderService.RETURNING)
    public boolean returningOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }

    /**
     * @description: 客户关闭工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = IOrderService.RETURNING, target = IOrderService.CLOSED)
    public boolean closedOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return true;
    }


}