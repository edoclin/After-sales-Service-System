package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.ConvertException;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ObjectUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.config.OrderStateMachineBuilder;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.config.yaml.bean.OrderConfig;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.entity.enums.OrderTypeEnum;
import com.mi.aftersales.entity.enums.OrderUploaderTypeEnum;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.*;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.form.ClientOrderForm;
import com.mi.aftersales.vo.form.FaultDescriptionForm;
import com.mi.aftersales.vo.message.OrderUploadMessage;
import com.mi.aftersales.vo.result.EngineerSimpleOrderVo;
import io.swagger.v3.oas.annotations.Operation;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.StateMachine;
import org.springframework.statemachine.persist.StateMachinePersister;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_LOG;
import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_UPLOAD;

/**
 * <p>
 * 工单 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/order")
public class OrderController {
    private static final Logger log = LoggerFactory.getLogger(OrderController.class);

    public static final String NAMESPACE_4_MACHINE_PERSIST = "machine:persist:";
    public static final String NAMESPACE_4_ORDER_LOCK = "order:lock:";
    @Resource
    private OrderConfig orderConfig;

    @Resource(name = "orderRedisPersister")
    private StateMachinePersister<OrderStatusEnum, OrderStatusChangeEventEnum, String> orderRedisPersister;
    @Resource
    private OrderStateMachineBuilder orderStateMachineBuilder;

    @Resource
    private IFapiaoService iFapiaoService;

    @Resource
    private IOrderService iOrderService;

    @Resource
    private ISkuService iSkuService;

    @Resource
    private ISpuService iSpuService;

    @Resource
    private ISpuCategoryService iSpuCategoryService;

    @Resource
    private IClientServiceCenterService iClientServiceCenterService;

    @Resource
    private RocketMQTemplate rocketmqTemplate;

    @Resource
    private RedisTemplate<String, String> redisTemplate;

    @Resource
    private IFileService iFileService;

    @Resource
    private RedissonClient redissonClient;

    @Resource
    private IOrderUploadService iOrderUploadService;


    @PostMapping(path = "/client/create")
    @Operation(summary = "客户创建工单", description = "客户创建工单")
    @Transactional
    public void postOrder(@RequestBody @Valid ClientOrderForm form) {

        Fapiao fapiao = iFapiaoService.getById(form.getFapiaoId());

        if (BeanUtil.isEmpty(fapiao) || !CharSequenceUtil.equals(fapiao.getCreatedId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("非法的发票Id！");
        }

        Sku sku = iSkuService.getById(form.getSkuId());

        if (BeanUtil.isEmpty(sku) || Boolean.FALSE.equals(sku.getVisible())) {
            throw new GracefulResponseException("非法的商品Sku！");
        }

        if (form.getFileIds().length > 3) {
            throw new GracefulResponseException("上传图片超出限制（3张）");
        }

        Order order = new Order();

        try {
            BeanUtil.copyProperties(form, order);
        } catch (ConvertException e) {
            throw new GracefulResponseException("订单类型不合法！");
        }

        if (order.getOrderType() == OrderTypeEnum.TO_SHOP && BeanUtil.isEmpty(iClientServiceCenterService.getById(order.getCenterId()))) {
            throw new GracefulResponseException("客户服务中心不存在");
        }

        if (order.getOrderType() == OrderTypeEnum.SEND_FOR) {
            order.setCenterId("");
        }

        if (LocalDateTimeUtil.now().isAfter(form.getArrivalTime())) {
            throw new GracefulResponseException("预约时间不能早于当前时间！");
        }


        order.setClientLoginId(StpUtil.getLoginIdAsString());
        try {
            iOrderService.save(order);
            if (sendEvent(statusFlow(OrderStatusChangeEventEnum.CLIENT_COMPLETED_ORDER_CREATED, order.getOrderId()))) {
                throw new ServerErrorException();
            }

            // 关联工单文件
            Message<OrderUploadMessage> msg = MessageBuilder.withPayload(new OrderUploadMessage().setOrderId(order.getOrderId()).setFileIds(form.getFileIds()).setUploaderType(OrderUploaderTypeEnum.CLIENT)).build();
            rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_UPLOAD, msg);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }


    }


    @GetMapping(path = "/engineer/pending")
    @Operation(summary = "工程师查询待办工单", description = "工程师查询待办工单")
    public List<EngineerSimpleOrderVo> listPendingOrder() {
        // todo 检查角色
//        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        ArrayList<EngineerSimpleOrderVo> result = new ArrayList<>();
        ArrayList<Order> orders = new ArrayList<>();
        // 按工单提交先后顺序，一次只能查询topN个
        Set<String> pendingOrders = redisTemplate.opsForZSet().range(IOrderService.NAMESPACE_4_PENDING_ORDER, 0, orderConfig.getTopN() - 1);
        if (ObjectUtil.isNotNull(pendingOrders)) {
            pendingOrders.forEach(orderId -> {
                Order order = iOrderService.getById(orderId);
                if (BeanUtil.isNotEmpty(order)) {
                    orders.add(order);
                }
            });
            orders.forEach(order -> {
                EngineerSimpleOrderVo item = new EngineerSimpleOrderVo();
                BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
                Sku sku = iSkuService.getById(order.getSkuId());
                item.setSkuDisplayName(sku.getSkuDisplayName());
                Spu spu = iSpuService.getById(sku.getSpuId());
                item.setSpuName(spu.getSpuName());
                item.setCategories(iSpuCategoryService.listAllSpuCategoryName(spu.getCategoryId()));
                item.setOrderType(order.getOrderType().getDesc());
                result.add(item);
            });
        }
        return result;
    }

    @GetMapping(path = "/engineer/accept/{orderId}")
    @Operation(summary = "工程师接受工单", description = "工程师接受工单")
    @CheckLogin
    public void engineerAcceptOrder(@PathVariable String orderId) {
        // todo check role

        Set<String> orderRange = redisTemplate.opsForZSet().range(IOrderService.NAMESPACE_4_PENDING_ORDER, 0, orderConfig.getTopN());
        if (CollUtil.isEmpty(orderRange)) {
            throw new GracefulResponseException("当前待办工单为空！");
        }
        if (Boolean.TRUE.equals(orderRange.contains(orderId))) {
            RLock fairLock = redissonClient.getFairLock(NAMESPACE_4_ORDER_LOCK + orderId);
            if (fairLock.tryLock()) {
                if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT, orderId))) {
                    throw new GracefulResponseException("状态转换非法！");
                }
                fairLock.unlock();
            } else {
                throw new GracefulResponseException("该工单已被受理！");
            }
        } else {
            throw new GracefulResponseException("该工单已被受理！");
        }
    }

    @GetMapping(path = "/engineer/checking/{orderId}")
    @Operation(summary = "工程师开始检测", description = "工程师开始检测")
    @CheckLogin
    public void startRepairMachine(@PathVariable String orderId) {
        // todo check role
        Order order = iOrderService.getById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("非法的工单Id！");
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("该工单不属于当前用户！");
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING, orderId))) {
            throw new GracefulResponseException("状态转换非法！");
        }

    }


    @PutMapping(path = "/engineer/faultDesc")
    @Operation(summary = "工程师上传故障描述", description = "工程师拆机维修，工程师故障描述")
    public void faultDescription(@RequestBody @Valid FaultDescriptionForm form) {
        // todo check role

        Order order = iOrderService.getById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("该工单已撤销");
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("该工单不属于当前用户！");
        }

        order.setEngineerFaultDesc(form.getEngineerFaultDesc());
        order.setEngineerNotice(form.getEngineerNotice());
        try {
            iOrderService.updateById(order);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }

        // 用消息队列写OrderStatus日志，更新Order表就对应一条记录
        OrderStatusLog orderStatusLog = new OrderStatusLog();
        orderStatusLog.setOrderId(order.getOrderId());
        orderStatusLog.setOrderStatus(order.getOrderStatus());
        orderStatusLog.setStatusDetail("工程师上传故障描述");

        Message<OrderStatusLog> msg = MessageBuilder.withPayload(orderStatusLog).build();
        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_LOG, msg);
    }

    /*
     * 订单状态流程转换
     * */
    public static Message<OrderStatusChangeEventEnum> statusFlow(OrderStatusChangeEventEnum payload, String orderId) {
        return MessageBuilder.withPayload(payload).setHeader(IOrderService.STATE_MACHINE_HEADER_ORDER_NAME, orderId).build();
    }


    /**
     * 发送状态转换事件
     *
     * @param message
     * @return
     */
    public synchronized boolean sendEvent(Message<OrderStatusChangeEventEnum> message) {
        boolean result;
        StateMachine<OrderStatusEnum, OrderStatusChangeEventEnum> stateMachine = null;
        try {
            stateMachine = orderStateMachineBuilder.build();
            stateMachine.start();
            if (Boolean.TRUE.equals(redisTemplate.hasKey(NAMESPACE_4_MACHINE_PERSIST + message.getHeaders().get(IOrderService.STATE_MACHINE_HEADER_ORDER_NAME)))) {
                // 存在持久化对象则恢复
                orderRedisPersister.restore(stateMachine, NAMESPACE_4_MACHINE_PERSIST + message.getHeaders().get(IOrderService.STATE_MACHINE_HEADER_ORDER_NAME));
            }
            result = stateMachine.sendEvent(message);
            orderRedisPersister.persist(stateMachine, NAMESPACE_4_MACHINE_PERSIST + message.getHeaders().get(IOrderService.STATE_MACHINE_HEADER_ORDER_NAME));
        } catch (Exception e) {
            throw new GracefulResponseException(e.getMessage());
        } finally {
            if (stateMachine != null) {
                stateMachine.stop();
            }

            if (stateMachine.isComplete()) {
                // todo 工单结束，删除持久化
            }
        }
        return result;
    }
}
