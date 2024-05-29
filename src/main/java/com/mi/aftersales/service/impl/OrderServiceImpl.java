package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.convert.ConvertException;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ObjectUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.config.statemachine.OrderStateMachineBuilder;
import com.mi.aftersales.config.yaml.bean.OrderConfig;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.entity.enums.*;
import com.mi.aftersales.exception.graceful.*;
import com.mi.aftersales.mapper.*;
import com.mi.aftersales.service.*;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.form.*;
import com.mi.aftersales.vo.message.OrderUploadMessage;
import com.mi.aftersales.vo.result.*;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.ObjectStateMachine;
import org.springframework.statemachine.persist.StateMachinePersister;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.util.RocketMqTopic.*;
import static com.mi.aftersales.service.IMaterialService.NAMESPACE_4_MATERIAL_LOCK;

/**
 * <p>
 * 工单 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements IOrderService {
    public static final String NAMESPACE_4_ORDER_LOCK = "order:lock:";
    public static final String NAMESPACE_4_MATERIAL_LOCK = "material:lock:";
    @Resource
    private OrderConfig orderConfig;

    @Resource
    private IMaterialService iMaterialService;

    @Resource(name = "orderRedisPersister")
    private StateMachinePersister<OrderStatusEnum, OrderStatusChangeEventEnum, String> orderRedisPersister;

    @Resource
    private OrderStateMachineBuilder orderStateMachineBuilder;

    @Resource
    private IFapiaoService iFapiaoService;

    @Resource
    private IOrderStatusLogService iOrderStatusLogService;

    @Resource
    private OrderMapper orderMapper;

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

    @Override
    public List<ClientOrderSimpleVo> listClientOrders(ConditionQuery query, String loginId) {
        QueryWrapper<Order> wrapper = QueryUtil.buildWrapper(query, Order.class);
        wrapper.eq("client_login_id", loginId);
        List<ClientOrderSimpleVo> result = new ArrayList<>();
        orderMapper.selectList(wrapper).forEach(order -> {
            ClientOrderSimpleVo item = new ClientOrderSimpleVo();
            BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
            item.setOrderStatus(order.getOrderStatus().getDesc());
            item.setOrderStatusValue(order.getOrderStatus().getValue());
            result.add(item);
        });
        return result;
    }

    @Override
    public ClientOrderDetailVo getClientOrderDetail(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalLoginIdException();
        }

        ClientOrderDetailVo clientOrderDetailVo = new ClientOrderDetailVo();
        BeanUtil.copyProperties(order, clientOrderDetailVo, DateUtil.copyDate2yyyyMMddHHmm());
        clientOrderDetailVo.setOrderStatus(order.getOrderStatus().getDesc());
        clientOrderDetailVo.setOrderStatusValue(order.getOrderStatus().getValue());

        // 状态日志
        iOrderStatusLogService.lambdaQuery().eq(OrderStatusLog::getOrderId, order.getOrderId())
                .orderByAsc(OrderStatusLog::getOrderStatus).list().forEach(log -> {
                    ClientOrderStatusLogVo logVo = new ClientOrderStatusLogVo();
                    BeanUtil.copyProperties(log, logVo, DateUtil.copyDate2yyyyMMddHHmm());
                    logVo.setOrderStatus(log.getOrderStatus().getDesc());
                    logVo.setOrderStatusValue(log.getOrderStatus().getValue());
                    clientOrderDetailVo.getStatusLogs().add(logVo);
                });

        // 客户上传文件
        iOrderUploadService.lambdaQuery().eq(OrderUpload::getOrderId, order.getOrderId())
                .eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.CLIENT).list().forEach(file -> {
                    File byId = iFileService.getById(file.getFileId());
                    if (BeanUtil.isNotEmpty(byId)) {
                        clientOrderDetailVo.getClientFileUrl()
                                .add(new FileVo().setUrl(COSUtil.generateAccessUrl(byId.getAccessKey())).setType("image").setFileId(file.getFileId()));
                    }
                });

        // 工程师上传文件
        iOrderUploadService.lambdaQuery().eq(OrderUpload::getOrderId, order.getOrderId())
                .eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER).list().forEach(file -> {
                    File byId = iFileService.getById(file.getFileId());
                    if (BeanUtil.isNotEmpty(byId)) {
                        clientOrderDetailVo.getEngineerFileUrl()
                                .add(new FileVo().setUrl(COSUtil.generateAccessUrl(byId.getAccessKey())).setType("video").setFileId(file.getFileId()));
                    }
                });

        return clientOrderDetailVo;
    }

    @Override
    public void createOrder(ClientOrderForm form, String loginId) {
        Fapiao fapiao = iFapiaoService.getById(form.getFapiaoId());

        if (BeanUtil.isEmpty(fapiao) || !CharSequenceUtil.equals(fapiao.getCreatedId(), loginId)) {
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

        order.setClientLoginId(loginId);
        try {
            orderMapper.insert(order);
            if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.CLIENT_COMPLETED_ORDER_CREATED, order.getOrderId()))) {
                log.error(CharSequenceUtil.format("工单（{}）状态转换失败", order.getOrderId()));
                throw new ServerErrorException();
            }

            // 关联工单文件
            Message<OrderUploadMessage> msg = MessageBuilder.withPayload(new OrderUploadMessage()
                    .setOrderId(order.getOrderId()).setFileIds(form.getFileIds()).setUploaderType(OrderUploaderTypeEnum.CLIENT)).build();
            rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_UPLOAD, msg);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public List<EngineerSimpleOrderVo> listPendingOrders() {
        ArrayList<EngineerSimpleOrderVo> result = new ArrayList<>();
        ArrayList<Order> orders = new ArrayList<>();
        // 按工单提交先后顺序，一次只能查询topN个
        Set<String> pendingOrders = redisTemplate.opsForZSet().range(NAMESPACE_4_PENDING_ORDER, 0, orderConfig.getTopN() - 1);
        if (ObjectUtil.isNotNull(pendingOrders)) {
            pendingOrders.forEach(orderId -> {
                Order order = orderMapper.selectById(orderId);
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

    @Override
    public void acceptOrder(String orderId, String loginId) {
        Set<String> orderRange = redisTemplate.opsForZSet().range(NAMESPACE_4_PENDING_ORDER, 0, orderConfig.getTopN());
        if (orderRange == null || !orderRange.contains(orderId)) {
            throw new GracefulResponseException("该工单已被受理！");
        }
        RLock fairLock = redissonClient.getFairLock(NAMESPACE_4_ORDER_LOCK + orderId);
        try {
            if (fairLock.tryLock(10, TimeUnit.SECONDS)) {
                if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT, orderId))) {
                    throw new IllegalOrderStatusFlowException();
                }
            } else {
                throw new GracefulResponseException("抢单失败！");
            }
        } catch (InterruptedException e) {
            log.error(e.getMessage());
            throw new GracefulResponseException("抢单失败！");
        } finally {
            if (fairLock.isLocked()) {
                fairLock.unlock();
            }
        }
    }

    @Override
    public void startChecking(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING, orderId))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void uploadFaultDescription(FaultDescriptionForm form, String loginId) {
        Order order = orderMapper.selectById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        order.setEngineerFaultDesc(form.getEngineerFaultDesc());
        order.setEngineerNotice(form.getEngineerNotice());
        try {
            orderMapper.updateById(order);
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

    @Override
    public void confirmFee(OrderFeeConfirmForm form, String loginId) {
        Order order = orderMapper.selectById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        order.setManualFee(form.getManualFee());

        List<MiddleOrderMaterial> batch = new ArrayList<>();
        order.setMaterialFee(new BigDecimal("0"));
        form.getMaterials().forEach(materialNum -> {
            Material material = iMaterialService.getById(materialNum.getMaterialId());
            if (BeanUtil.isNotEmpty(material)) {
                order.setMaterialFee(order.getMaterialFee().add(material.getPrice().multiply(materialNum.getNum())));
                batch.add(new MiddleOrderMaterial().setOrderId(form.getOrderId()).setMaterialId(material.getMaterialId()).setMaterialAmount(materialNum.getNum()));
            }
        });

        try {
            orderMapper.updateById(order);
            if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_FEE_CONFIRM, order.getOrderId()))) {
                throw new IllegalOrderStatusFlowException();
            }

            // 异步更新工单物料
            Message<List<MiddleOrderMaterial>> msg = MessageBuilder.withPayload(batch).build();
            rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_MATERIAL, msg);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw e;
        }
    }

    @Override
    public void clientConfirmFee(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }
        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.CLIENT_CONFIRMING)
                .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId)
                .setHeader(CLIENT_CHOICE, OrderStatusChangeEventEnum.CLIENT_COMPLETED_FEE_CONFIRM).build();

        if (!sendEvent(build)) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void clientRejectRepair(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.CLIENT_CONFIRMING)
                .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId)
                .setHeader(CLIENT_CHOICE, OrderStatusChangeEventEnum.CLIENT_REJECT_REPAIR).build();

        if (!sendEvent(build)) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void applyMaterial(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_MATERIAL_CONFIRMING)
                .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId)
                .setHeader(ENGINEER_CHOICE, OrderStatusChangeEventEnum.ENGINEER_APPLIED_MATERIAL).build();

        if (!sendEvent(build)) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void distributeMaterial(MaterialDistributeForm form, String loginId) {
        // 当工单状态 == MATERIAL_APPLY时，库管开始处理申请
        Order order = orderMapper.selectById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        RLock fairLock = redissonClient.getFairLock(NAMESPACE_4_MATERIAL_LOCK);
        try {
            List<MaterialLog> materialLogs = new ArrayList<>();
            List<Material> batch = new ArrayList<>();
            if (fairLock.tryLock(30, TimeUnit.DAYS)) {
                // 修改库存
                form.getMaterials().forEach(materialNum -> {
                    Material material = iMaterialService.getById(materialNum.getMaterialId());
                    if (BeanUtil.isEmpty(material)) {
                        throw new IllegalMaterialIdException();
                    }

                    if (material.getStock().compareTo(materialNum.getNum()) > 0) {
                        material.setStock(material.getStock().subtract(materialNum.getNum()));
                    } else {
                        throw new GracefulResponseException(CharSequenceUtil.format("物料（{}）库存不足！", materialNum.getMaterialId()));
                    }

                    batch.add(material);
                    MaterialLog materialLog = new MaterialLog();
                    materialLog.setMaterialId(materialNum.getMaterialId()).setAction(MaterialActionEnum.STOCK_OUT)
                            .setDelta(materialNum.getNum()).setOperatorId(loginId)
                            .setLogDetail(CharSequenceUtil.format("工单（id：{}，工程师：{}）申请物料", order.getOrderId(), order.getEngineerLoginId()));
                    materialLogs.add(materialLog);
                });

                // 异步物料变动日志
                Message<List<MaterialLog>> msg = MessageBuilder.withPayload(materialLogs).build();
                rocketmqTemplate.send(ROCKETMQ_TOPIC_4_MATERIAL_LOG, msg);
            }
        } catch (GracefulResponseException e) {
            log.error(e.getMsg());
            throw new GracefulResponseException(e.getMsg());
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        } finally {
            if (fairLock.isLocked()) {
                fairLock.unlock();
            }
        }
        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.MANAGER_DISTRIBUTED_MATERIAL, order.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void startRepair(String orderId, Boolean material, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }
        Message<OrderStatusChangeEventEnum> build;

        if (material) {
            build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_RECEIVED_MATERIAL)
                    .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).build();
        } else {
            build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_MATERIAL_CONFIRMING)
                    .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId)
                    .setHeader(ENGINEER_CHOICE, OrderStatusChangeEventEnum.ENGINEER_START_REPAIR).build();
        }
        if (!sendEvent(build)) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void startRechecking(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_START_RECHECK, order.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void finishRepair(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RECHECK, order.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void returnItem(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RETURN, order.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public void closeOrder(String orderId, String loginId) {
        Order order = orderMapper.selectById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (!sendEvent(statusFlow(OrderStatusChangeEventEnum.CLIENT_CLOSED, order.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    @Override
    public Message<OrderStatusChangeEventEnum> statusFlow(OrderStatusChangeEventEnum payload, String orderId) {
        return MessageBuilder.withPayload(payload).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).build();
    }

    @Override
    public synchronized boolean sendEvent(Message<OrderStatusChangeEventEnum> message) {
        boolean result;
        ObjectStateMachine<OrderStatusEnum, OrderStatusChangeEventEnum> stateMachine = null;
        try {
            stateMachine = orderStateMachineBuilder.build();
            stateMachine.start();
            if (Boolean.TRUE.equals(redisTemplate.hasKey("machine:persist:" + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME)))) {
                // 存在持久化对象则恢复
                orderRedisPersister.restore(stateMachine, "machine:persist:" + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
            }
            result = stateMachine.sendEvent(message);
            orderRedisPersister.persist(stateMachine, "machine:persist:" + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
        } catch (Exception e) {
            throw new GracefulResponseException(e.getMessage());
        } finally {
            if (stateMachine != null) {
                // todo 工单结束，删除持久化，目前还存在问题！！！
                if (OrderStatusEnum.CLOSED.equals(stateMachine.getState().getId())) {
                    redisTemplate.delete("machine:persist:" + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
                }
                stateMachine.stop();
            }
        }
        return result;
    }
}
