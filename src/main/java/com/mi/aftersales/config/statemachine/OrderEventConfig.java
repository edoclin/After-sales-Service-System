package com.mi.aftersales.config.statemachine;

import cn.dev33.satoken.SaManager;
import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.config.yaml.bean.OrderConfig;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.service.OrderService;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.message.PendingOrder;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.annotation.OnTransition;
import org.springframework.statemachine.annotation.WithStateMachine;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

import java.util.Set;

import static com.mi.aftersales.service.OrderService.NAMESPACE_4_PENDING_ORDER;
import static com.mi.aftersales.service.OrderService.STATE_MACHINE_HEADER_CATEGORY_ID;
import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_LOG;
import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_SMS;


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

    @Resource
    private ISpuCategoryRepository iSpuCategoryRepository;

    @Resource
    private RedisTemplate<String, Object> redisTemplate;

    @Resource
    private RocketMQTemplate rocketmqTemplate;

    @Resource
    private OrderConfig orderConfig;

    public void sendSms(String orderId) {
        Message<String> msg = MessageBuilder.withPayload(orderId).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_SMS, msg);
    }


    /**
     * @description: 客户完成工单创建
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/
    @OnTransition(source = OrderService.CREATED, target = OrderService.WAITING)
    public boolean createOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get(OrderService.STATE_MACHINE_HEADER_ORDER_NAME));

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        order.setOrderStatus(OrderStatusEnum.WAITING);
        OrderStatusLog orderStatusLog = new OrderStatusLog();

        orderStatusLog.setCreatedId(order.getClientLoginId()).setOrderId(order.getOrderId()).setOrderStatus(order.getOrderStatus()).setStatusDetail(CharSequenceUtil.format("工单创建成功，等待工程师处理！"));


        if (Boolean.FALSE.equals(iOrderRepository.updateById(order))) {
            throw new ServerErrorException();
        }

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);
        sendSms(order.getOrderId());


        Integer spuCategoryId = (Integer) message.getHeaders().get(STATE_MACHINE_HEADER_CATEGORY_ID);

        // 加入待办工单，设置时间戳
        PendingOrder pendingOrder = new PendingOrder();
        pendingOrder
                .setCreatedTime(DateUtil.yyyyMMddHHmm(order.getCreatedTime()))
                .setOrderId(order.getOrderId())
                .setCategories(iSpuCategoryRepository.listAllSpuCategoryName(spuCategoryId));

        redisTemplate.opsForZSet().add(NAMESPACE_4_PENDING_ORDER, pendingOrder, System.currentTimeMillis());
        return Boolean.TRUE;
    }

    /**
     * @description: 工程师接受处理工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:31
     **/

    @OnTransition(source = OrderService.WAITING, target = OrderService.ACCEPTED)
    public boolean acceptOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get(OrderService.STATE_MACHINE_HEADER_ORDER_NAME));

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        order.setOrderStatus(OrderStatusEnum.ACCEPTED);
        order.setEngineerLoginId(StpUtil.getLoginIdAsString());

        OrderStatusLog orderStatusLog = new OrderStatusLog();

        orderStatusLog.setOrderId(order.getOrderId()).setOrderStatus(order.getOrderStatus()).setStatusDetail(CharSequenceUtil.format("工单已被受理，等待工程师（{}）处理！", StpUtil.getLoginIdAsString()));

        if (Boolean.FALSE.equals(iOrderRepository.updateById(order))) {
            throw new ServerErrorException();
        }

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);
        // 移除待办工单
        Set<Object> pendingOrders;
        for (int i = 0; ; i += orderConfig.getTopN()) {
            pendingOrders = redisTemplate.opsForZSet().range(NAMESPACE_4_PENDING_ORDER, i, i + orderConfig.getTopN() - 1);
            if (CollUtil.isEmpty(pendingOrders)) {
                break;
            }
            PendingOrder delete = (PendingOrder) CollUtil.findOne(pendingOrders, item -> CharSequenceUtil.equals(((PendingOrder) item).getOrderId(), order.getOrderId()));
            if (BeanUtil.isNotEmpty(delete)) {
                redisTemplate.opsForZSet().remove(NAMESPACE_4_PENDING_ORDER, delete);
                sendSms(order.getOrderId());
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    /**
     * @description: 工程师开始检测
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.ACCEPTED, target = OrderService.CHECKING)
    public boolean checkingOrderTransition(Message<OrderStatusChangeEventEnum> message) {

        Order order = iOrderRepository.getById((String) message.getHeaders().get("order-id"));

        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("工单状态转换：工单Id不合法！");
        }

        order.setOrderStatus(OrderStatusEnum.CHECKING);

        OrderStatusLog orderStatusLog = new OrderStatusLog();

        orderStatusLog.setOrderId(order.getOrderId()).setOrderStatus(order.getOrderStatus());


        if (Boolean.FALSE.equals(iOrderRepository.updateById(order))) {
            throw new ServerErrorException();
        }

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);

        return Boolean.TRUE;
    }

    /**
     * @description: 工程师完成计费，等待用户确认
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.CHECKING, target = OrderService.FEE_CONFIRMING)
    public boolean confirmingFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.FEE_CONFIRMING, "工程师检测完成，发送费用账单");
    }

    /**
     * @description: 用户确认维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.FEE_CONFIRMING, target = OrderService.FEE_CONFIRMED)
    public boolean confirmedFeeOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.FEE_CONFIRMED, OrderStatusEnum.FEE_CONFIRMED.getDesc());
    }

    /**
     * @description: 工程师申请物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.FEE_CONFIRMED, target = OrderService.MATERIAL_APPLYING)
    public boolean applyingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.MATERIAL_APPLYING, OrderStatusEnum.MATERIAL_APPLYING.getDesc());
    }

    /**
     * @description: 库管分发物料
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.MATERIAL_APPLYING, target = OrderService.MATERIAL_DISTRIBUTING)
    public boolean distributingMaterialOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.MATERIAL_DISTRIBUTING, OrderStatusEnum.MATERIAL_DISTRIBUTING.getDesc());
    }

    /**
     * @description: 工程师开始维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.MATERIAL_DISTRIBUTING, target = OrderService.REPAIRING)
    public boolean repairOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.REPAIRING, OrderStatusEnum.REPAIRING.getDesc());
    }

    /**
     * @description: 工程师开始复检
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.REPAIRING, target = OrderService.RE_CHECKING)
    public boolean reCheckingOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RE_CHECKING, OrderStatusEnum.RE_CHECKING.getDesc());
    }

    /**
     * @description: 工程师完成复检，发起支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.RE_CHECKING, target = OrderService.TO_BE_PAID)
    public boolean toBePaidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.TO_BE_PAID, OrderStatusEnum.TO_BE_PAID.getDesc());
    }

    /**
     * @description: 客户完成支付
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.TO_BE_PAID, target = OrderService.PAID)
    public boolean paidOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.PAID, OrderStatusEnum.PAID.getDesc());
    }

    /**
     * @description: 用户拒绝维修
     * @return:
     * @author: edoclin
     * @created: 2024/5/20 21:50
     **/
    @OnTransition(source = OrderService.FEE_CONFIRMING, target = OrderService.RETURNING)
    public boolean rejectRepairTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RETURNING, "用户拒绝维修！");
    }


    /**
     * @description: 用户完成支付，工程师返回物品
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.PAID, target = OrderService.RETURNING)
    public boolean returningOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.RETURNING, "用户完成支付，工程师返回物品！");
    }

    /**
     * @description: 客户关闭工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.RETURNING, target = OrderService.CLOSED)
    public boolean closedOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.CLOSED, OrderStatusEnum.CLOSED.getDesc());
    }

    /**
     * @description: 客户关闭工单
     * @return:
     * @author: edoclin
     * @created: 2024/5/18 17:32
     **/
    @OnTransition(source = OrderService.FEE_CONFIRMED, target = OrderService.REPAIRING)
    public boolean startRepairOrderTransition(Message<OrderStatusChangeEventEnum> message) {
        return updateOrderStatus(message, OrderStatusEnum.REPAIRING, OrderStatusEnum.REPAIRING.getDesc());
    }


    private boolean updateOrderStatus(Message<OrderStatusChangeEventEnum> message, OrderStatusEnum target, String detail) {
        Order order = iOrderRepository.getById((String) message.getHeaders().get("order-id"));
        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("工单状态转换：工单Id不合法！");
        }
        order.setOrderStatus(target);
        OrderStatusLog orderStatusLog = new OrderStatusLog();
        orderStatusLog.setOrderId(order.getOrderId()).setOrderStatus(order.getOrderStatus());
        if (Boolean.FALSE.equals(iOrderRepository.updateById(order))) {
            throw new ServerErrorException();
        }
        orderStatusLog.setStatusDetail(detail);

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);

        sendSms(order.getOrderId());
        return Boolean.TRUE;
    }
}