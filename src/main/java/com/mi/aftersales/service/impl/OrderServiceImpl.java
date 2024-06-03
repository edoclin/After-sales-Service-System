package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.ConvertException;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.yaml.bean.OrderConfig;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.*;
import com.mi.aftersales.exception.graceful.*;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.message.PendingOrderMessage;
import com.mi.aftersales.pojo.vo.*;
import com.mi.aftersales.pojo.vo.form.*;
import com.mi.aftersales.repository.*;
import com.mi.aftersales.service.OrderService;
import com.mi.aftersales.statemachine.OrderStateMachineBuilder;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.ObjectStateMachine;
import org.springframework.statemachine.persist.StateMachinePersister;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.util.RocketMqTopic.*;

/**
 * <p>
 * 工单 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class OrderServiceImpl implements OrderService {
    public static final String NAMESPACE_4_ORDER_LOCK = "order:lock:";
    public static final String NAMESPACE_4_MATERIAL_LOCK = "material:lock:";
    private static final Logger log = LoggerFactory.getLogger(OrderServiceImpl.class);
    @Resource
    private OrderConfig orderConfig;

    @Resource
    private IMaterialRepository iMaterialRepository;


    @Resource(name = "orderRedisPersister")
    private StateMachinePersister<OrderStatusEnum, OrderStatusChangeEventEnum, String> orderRedisPersister;

    @Resource
    private OrderStateMachineBuilder orderStateMachineBuilder;

    @Resource
    private IFapiaoRepository iFapiaoRepository;

    @Resource
    private IOrderStatusLogRepository iOrderStatusLogRepository;

    @Resource
    private IOrderRepository iOrderRepository;

    @Resource
    private ISkuRepository iSkuRepository;

    @Resource
    private ISpuRepository iSpuRepository;

    @Resource
    private ISpuCategoryRepository iSpuCategoryRepository;

    @Resource
    private IClientServiceCenterRepository iClientServiceCenterRepository;

    @Resource
    private RocketMQTemplate rocketmqTemplate;

    @Resource
    private RedisTemplate<String, Object> redisTemplate;

    @Resource
    private IFileRepository iFileRepository;

    @Resource
    private RedissonClient redissonClient;

    @Resource
    private IOrderUploadRepository iOrderUploadRepository;

    /**
     * @description: 客户查询工单列表
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:46
     **/
    @Override
    public List<ClientOrderSimpleVo> listClientOrders(ConditionQuery query, String loginId) {
        QueryWrapper<Order> wrapper = QueryUtil.buildWrapper(query, Order.class);
        wrapper = wrapper.eq("client_login_id", loginId).orderByDesc("created_time");
        List<ClientOrderSimpleVo> result = new ArrayList<>();
        iOrderRepository.list(wrapper).forEach(order -> {
            ClientOrderSimpleVo item = new ClientOrderSimpleVo();
            BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
            item.setOrderStatus(order.getOrderStatus().getDesc());
            item.setOrderStatusValue(order.getOrderStatus().getValue());
            result.add(item);
        });
        return result;
    }

    /**
     * @description: 查询工单详情
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:46
     **/
    @Override
    public OrderDetailVo getClientOrderDetail(String orderId, String loginId, Boolean isClient) {
        Order order = iOrderRepository.getById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if ((Boolean.TRUE.equals(isClient) && !CharSequenceUtil.equals(order.getClientLoginId(), loginId)) ||
                Boolean.FALSE.equals(isClient) && !CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)
        ) {
            throw new IllegalLoginIdException();
        }

        OrderDetailVo orderDetailVo = new OrderDetailVo();
        BeanUtil.copyProperties(order, orderDetailVo, DateUtil.copyDate2yyyyMMddHHmm());
        orderDetailVo.setOrderStatus(order.getOrderStatus().getDesc());
        orderDetailVo.setOrderStatusValue(order.getOrderStatus().getValue());

        // 状态日志
        iOrderStatusLogRepository.lambdaQuery().eq(OrderStatusLog::getOrderId, order.getOrderId()).orderByAsc(OrderStatusLog::getOrderStatus).list().forEach(innerLog -> {
            ClientOrderStatusLogVo logVo = new ClientOrderStatusLogVo();
            BeanUtil.copyProperties(innerLog, logVo, DateUtil.copyDate2yyyyMMddHHmm());
            logVo.setOrderStatus(innerLog.getOrderStatus().getDesc());
            logVo.setOrderStatusValue(innerLog.getOrderStatus().getValue());
            orderDetailVo.getStatusLogs().add(logVo);
        });

        // 客户上传文件
        iOrderUploadRepository.lambdaQuery().eq(OrderUpload::getOrderId, order.getOrderId()).eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.CLIENT).list().forEach(file -> {
            File byId = iFileRepository.getById(file.getFileId());
            if (BeanUtil.isNotEmpty(byId)) {
                orderDetailVo.getClientFileUrl().add(new FileVo().setUrl(COSUtil.generateAccessUrl(byId.getAccessKey())).setType("image").setFileId(file.getFileId()));
            }
        });

        // 工程师上传文件（图片）
        iOrderUploadRepository.lambdaQuery()
                .eq(OrderUpload::getOrderId, order.getOrderId())
                .eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER)
                .eq(OrderUpload::getFileType, OrderUploadFileTypeEnum.IMAGE)
                .list().forEach(file -> {
                    File byId = iFileRepository.getById(file.getFileId());
                    if (BeanUtil.isNotEmpty(byId)) {
                        orderDetailVo.getEngineerImageUrl().add(new FileVo()
                                .setUrl(COSUtil.generateAccessUrl(byId.getAccessKey()))
                                .setType(OrderUploadFileTypeEnum.IMAGE.name())
                                .setFileId(file.getFileId()));
                    }
                });

        // 工程师上传文件（视频）
        iOrderUploadRepository.lambdaQuery()
                .eq(OrderUpload::getOrderId, order.getOrderId())
                .eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER)
                .eq(OrderUpload::getFileType, OrderUploadFileTypeEnum.VIDEO)
                .list().forEach(file -> {
                    File byId = iFileRepository.getById(file.getFileId());
                    if (BeanUtil.isNotEmpty(byId)) {
                        orderDetailVo.getEngineerVideoUrl().add(new FileVo()
                                .setUrl(COSUtil.generateAccessUrl(byId.getAccessKey()))
                                .setType(OrderUploadFileTypeEnum.VIDEO.name())
                                .setFileId(file.getFileId()));
                    }
                });

        return orderDetailVo;
    }

    @Override
    @Transactional
    public void createOrder(ClientOrderFormVo form, String loginId) {
        Fapiao fapiao = iFapiaoRepository.getById(form.getFapiaoId());
        if (BeanUtil.isEmpty(fapiao) || !CharSequenceUtil.equals(fapiao.getCreatedId(), loginId)) {
            throw new IllegalFapiaoIdException();
        }

        Sku sku = iSkuRepository.getById(form.getSkuId());

        if (BeanUtil.isEmpty(sku) || Boolean.FALSE.equals(sku.getVisible())) {
            throw new IllegalSkuIdException();
        }

        Spu spu = iSpuRepository.getById(sku.getSpuId());

        if (form.getFileIds().length > 3) {
            throw new GracefulResponseException("上传图片超出限制（3张）");
        }

        Order order = new Order();

        try {
            BeanUtil.copyProperties(form, order);
        } catch (ConvertException e) {
            throw new GracefulResponseException("订单类型不合法！");
        }

        if (order.getOrderType() == OrderTypeEnum.TO_SHOP && BeanUtil.isEmpty(iClientServiceCenterRepository.getById(order.getCenterId()))) {
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
            iOrderRepository.save(order);

            Message<OrderStatusChangeEventEnum> message = MessageBuilder
                    .withPayload(OrderStatusChangeEventEnum.CLIENT_COMPLETED_ORDER_CREATED)
                    .setHeader(STATE_MACHINE_HEADER_ORDER_NAME, order.getOrderId())
                    .setHeader(STATE_MACHINE_HEADER_CATEGORY_ID, spu.getCategoryId())
                    .build();

            if (Boolean.FALSE.equals(sendEvent(message))) {
                throw new IllegalOrderStatusFlowException();
            }
            List<OrderUpload> batch = new ArrayList<>();
            for (String fileId : form.getFileIds()) {
                File byId = iFileRepository.getById(fileId);

                if (BeanUtil.isEmpty(byId)) {
                    throw new IllegalFileIdException();
                }
                OrderUpload orderUpload = new OrderUpload();
                orderUpload.setUploaderType(OrderUploaderTypeEnum.CLIENT);
                orderUpload.setOrderId(order.getOrderId());
                orderUpload.setFileType(OrderUploadFileTypeEnum.IMAGE);
                orderUpload.setFileId(fileId);
                orderUpload.setCreatedId(StpUtil.getLoginIdAsString());
                batch.add(orderUpload);
            }

            Message<List<OrderUpload>> msg = MessageBuilder.withPayload(batch).build();
            rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_UPLOAD, msg);

        } catch (BaseCustomException e) {
            throw e;
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    /**
     * @description: 工程师查询待办工单
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:21
     **/
    @Override
    public List<PendingOrderSimple4EngineerVo> listPendingOrders(Integer spuCategoryId) {
        ArrayList<PendingOrderSimple4EngineerVo> result = new ArrayList<>();

//         按工单提交先后顺序，一次只能查询topN个
        Set<Object> pendingOrders;
        for (int i = 0; ; i += orderConfig.getTopN()) {
            pendingOrders = redisTemplate.opsForZSet().range(NAMESPACE_4_PENDING_ORDER, i, i + orderConfig.getTopN() - 1);
            if (CollUtil.isEmpty(pendingOrders)) {
                break;
            }
            for (Object obj : pendingOrders) {
                if (obj instanceof PendingOrderMessage pendingOrder) {
                    if (!CollUtil.contains(pendingOrder.getCategories(), item -> Objects.equals(item.getCategoryId(), spuCategoryId))) {
                        // 不属于查询分类
                        continue;
                    }
                    Order order = iOrderRepository.getById(pendingOrder.getOrderId());
                    if (BeanUtil.isNotEmpty(order)) {
                        PendingOrderSimple4EngineerVo item = new PendingOrderSimple4EngineerVo();
                        BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
                        Sku sku = iSkuRepository.getById(order.getSkuId());
                        item.setSkuDisplayName(sku.getSkuDisplayName());
                        Spu spu = iSpuRepository.getById(sku.getSpuId());
                        item.setSpuName(spu.getSpuName());
                        pendingOrder.getCategories().forEach(category -> item.getCategories().add(category.getCategoryName()));
                        item.setOrderType(order.getOrderType().getDesc());
                        result.add(item);
                        if (result.size() >= orderConfig.getTopN()) {
                            return result;
                        }
                    }
                }
            }
        }
        return result;
    }

    /**
     * @description: 工程师接受工单
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:21
     **/
    @Override
    public void acceptOrder(String orderId, String loginId) {
        boolean isPending = Boolean.FALSE;
        Set<Object> pendingOrders;
        for (int i = 0; ; i += orderConfig.getTopN()) {
            pendingOrders = redisTemplate.opsForZSet().range(NAMESPACE_4_PENDING_ORDER, i, i + orderConfig.getTopN() - 1);
            if (CollUtil.isEmpty(pendingOrders)) {
                break;
            }
            if (CollUtil.contains(pendingOrders, item -> CharSequenceUtil.equals(((PendingOrderMessage) item).getOrderId(), orderId))) {
                isPending = Boolean.TRUE;
                break;
            }
        }

        if (Boolean.FALSE.equals(isPending)) {
            throw new GracefulResponseException("该工单已被受理！");
        }
        RLock fairLock = redissonClient.getFairLock(NAMESPACE_4_ORDER_LOCK + orderId);
        try {
            if (fairLock.tryLock(10, TimeUnit.SECONDS)) {
                if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT, orderId)))) {
                    throw new IllegalOrderStatusFlowException();
                }
            } else {
                throw new GracefulResponseException("抢单失败！");
            }
        } catch (InterruptedException e) {
            log.error(e.getMessage());
            Thread.currentThread().interrupt();
            throw new ServerErrorException();
        } catch (BaseCustomException e) {
            log.warn(e.getMessage());
            throw e;
        } finally {
            if (fairLock.isLocked()) {
                fairLock.unlock();
            }
        }
    }

    /**
     * @description: 工程师查询所属工单列表
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:21
     **/
    @Override
    public PageResult<OrderSimple4EngineerVo> listEngineerOrder(ConditionQuery query) {

        PageResult<OrderSimple4EngineerVo> result = new PageResult<>();

        QueryWrapper<Order> wrapper = QueryUtil.buildWrapper(query, Order.class);

        wrapper = wrapper.eq("engineer_login_id", StpUtil.getLoginIdAsString()).orderByDesc("created_time");

        result.setTotal(iOrderRepository.count(wrapper));

        iOrderRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords()
                .forEach(order -> {
                    OrderSimple4EngineerVo item = new OrderSimple4EngineerVo();
                    BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
                    item.setOrderStatus(order.getOrderStatus().getDesc());
                    Sku sku = iSkuRepository.getById(order.getSkuId());
                    Spu spu = iSpuRepository.getById(sku.getSpuId());
                    item.setCategories(iSpuCategoryRepository.listAllSpuCategoryName(spu.getCategoryId()));
                    item.setOrderStatusValue(order.getOrderStatus().getValue());
                    item.setOrderType(order.getOrderType().getDesc());
                    result.getData().add(item);
                });
        return result;
    }

    /**
     * @description: 工程师上传检测前图片
     * @return:
     * @author: edoclin
     * @created: 2024/5/29 21:01
     **/
    @Override
    @Transactional
    public void engineerUploadImage(EngineerUploadFormVo form) {
        Order order = iOrderRepository.getById(form.getOrderId());

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), StpUtil.getLoginIdAsString())) {
            throw new IllegalOrderLoginIdException();
        }

        if (ArrayUtil.isEmpty(form.getFileIds()) || form.getFileIds().length > 3) {
            throw new GracefulResponseException("0<文件数量<=3");
        }

        List<OrderUpload> batch = new ArrayList<>();
        for (String fileId : form.getFileIds()) {
            File byId = iFileRepository.getById(fileId);

            if (BeanUtil.isEmpty(byId)) {
                throw new IllegalFileIdException();
            }
            OrderUpload orderUpload = new OrderUpload().setFileId(fileId).setUploaderType(OrderUploaderTypeEnum.ENGINEER).setOrderId(order.getOrderId()).setFileType(OrderUploadFileTypeEnum.IMAGE).setCreatedId(StpUtil.getLoginIdAsString());
            batch.add(orderUpload);
        }

        Message<List<OrderUpload>> msg = MessageBuilder.withPayload(batch).build();
        rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_ORDER_UPLOAD, msg);

    }

    /**
     * @description: 工程师开始检测
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:21
     **/
    @Override
    @Transactional
    public void startChecking(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        List<OrderUpload> orderUploads = iOrderUploadRepository.lambdaQuery().eq(OrderUpload::getOrderId, orderId).eq(OrderUpload::getFileType, OrderUploadFileTypeEnum.IMAGE).eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER).list();
        if (CollUtil.isEmpty(orderUploads)) {
            throw new GracefulResponseException("工程师未按要求上传检测前图片！");
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING, orderId)))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师上传故障描述
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:21
     **/
    @Override
    @Transactional
    public void uploadFaultDescription(FaultDescriptionFormVo form, String loginId) {
        Order order = iOrderRepository.getById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        order.setEngineerFaultDesc(form.getEngineerFaultDesc());
        order.setEngineerNotice(form.getEngineerNotice());
        try {
            iOrderRepository.updateById(order);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }

    }

    /**
     * @description: 工程师确认费用账单
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void confirmFee(OrderFeeConfirmFormVo form, String loginId) {
        Order order = iOrderRepository.getById(form.getOrderId());
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (CharSequenceUtil.isEmpty(order.getEngineerFaultDesc())) {
            throw new GracefulResponseException("尚未上传故障描述，无法确认费用！");
        }

        order.setManualFee(form.getManualFee());

        List<MiddleOrderMaterial> batch = new ArrayList<>();
        order.setMaterialFee(new BigDecimal("0"));
        // 这里只计算费用，不扣减库存
        form.getMaterials().forEach(materialNum -> {
            Material material = iMaterialRepository.getById(materialNum.getMaterialId());
            if (BeanUtil.isNotEmpty(material)) {
                order.setMaterialFee(order.getMaterialFee().add(material.getPrice().multiply(materialNum.getNum())));
                batch.add(new MiddleOrderMaterial().setOrderId(form.getOrderId()).setMaterialId(material.getMaterialId()).setMaterialAmount(materialNum.getNum()));
            }
        });

        try {
            iOrderRepository.updateById(order);

        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_FEE_CONFIRM, order.getOrderId())))) {
            throw new IllegalOrderStatusFlowException();
        }

        // 异步更新工单物料
        Message<List<MiddleOrderMaterial>> msg = MessageBuilder.withPayload(batch).build();
        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ORDER_MATERIAL, msg);
    }

    /**
     * @description: 客户确认费用
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void clientConfirmFee(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }
        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.CLIENT_CONFIRMING).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).setHeader(CLIENT_CHOICE, OrderStatusChangeEventEnum.CLIENT_COMPLETED_FEE_CONFIRM).build();

        if (Boolean.FALSE.equals(sendEvent(build))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 客户拒绝维修
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void clientRejectRepair(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }
        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.CLIENT_CONFIRMING).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).setHeader(CLIENT_CHOICE, OrderStatusChangeEventEnum.CLIENT_REJECT_REPAIR).build();

        if (Boolean.FALSE.equals(sendEvent(build))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师申请物料
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void applyMaterial(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        Message<OrderStatusChangeEventEnum> build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_MATERIAL_CONFIRMING).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).setHeader(ENGINEER_CHOICE, OrderStatusChangeEventEnum.ENGINEER_APPLIED_MATERIAL).build();

        if (Boolean.FALSE.equals(sendEvent(build))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 库管分发物料
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void distributeMaterial(MaterialDistributeFormVo form, String loginId) {
        // 当工单状态 == MATERIAL_APPLY时，库管开始处理申请
        Order order = iOrderRepository.getById(form.getOrderId());
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
                    Material material = iMaterialRepository.getById(materialNum.getMaterialId());
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
                    materialLog.setMaterialId(materialNum.getMaterialId()).setAction(MaterialActionEnum.STOCK_OUT).setDelta(materialNum.getNum()).setOperatorId(loginId).setLogDetail(CharSequenceUtil.format("工单（id：{}，工程师：{}）申请物料", order.getOrderId(), order.getEngineerLoginId()));
                    materialLogs.add(materialLog);
                });

                // 异步物料变动日志
                Message<List<MaterialLog>> msg = MessageBuilder.withPayload(materialLogs).build();
                rocketmqTemplate.send(ROCKETMQ_TOPIC_4_MATERIAL_LOG, msg);
            }
        } catch (BaseCustomException e) {
            throw e;
        } catch (InterruptedException e) {
            log.error(e.getMessage());
            Thread.currentThread().interrupt();
            throw new ServerErrorException();
        } finally {
            if (fairLock.isLocked()) {
                fairLock.unlock();
            }
        }
        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.MANAGER_DISTRIBUTED_MATERIAL, order.getOrderId())))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师开始维修
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void startRepair(String orderId, Boolean material, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }
        Message<OrderStatusChangeEventEnum> build;

        if (Boolean.TRUE.equals(material)) {
            // 来自收到物料的状态转换
            build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_RECEIVED_MATERIAL).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).build();
        } else {
            // 来自直接开始维修的状态转换
            build = MessageBuilder.withPayload(OrderStatusChangeEventEnum.ENGINEER_MATERIAL_CONFIRMING).setHeader(STATE_MACHINE_HEADER_ORDER_NAME, orderId).setHeader(ENGINEER_CHOICE, OrderStatusChangeEventEnum.ENGINEER_START_REPAIR).build();
        }
        if (Boolean.FALSE.equals(sendEvent(build))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师开始复检
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:22
     **/
    @Override
    @Transactional
    public void startRechecking(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_START_RECHECK, order.getOrderId())))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师上传维修视频文件
     * @return:
     * @author: edoclin
     * @created: 2024/5/29 20:40
     **/
    @Override
    @Transactional
    public void engineerUploadVideo(EngineerUploadFormVo form) {
        Order order = iOrderRepository.getById(form.getOrderId());

        if (ArrayUtil.isEmpty(form.getFileIds()) || form.getFileIds().length != 1) {
            throw new GracefulResponseException("文件数量非法！");
        }
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (CollUtil.isNotEmpty(iOrderUploadRepository.lambdaQuery().eq(OrderUpload::getOrderId, form.getOrderId()).eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER).list())) {
            throw new GracefulResponseException("已完成文件上传！");
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), StpUtil.getLoginIdAsString())) {
            throw new IllegalOrderLoginIdException();
        }

        if (BeanUtil.isEmpty(iFileRepository.getById(form.getFileIds()[0]))) {
            throw new IllegalFileIdException();
        }

        OrderUpload orderUpload = new OrderUpload();
        orderUpload.setUploaderType(OrderUploaderTypeEnum.ENGINEER).setOrderId(form.getOrderId()).setFileId(form.getFileIds()[0]).setFileType(OrderUploadFileTypeEnum.VIDEO);

        try {
            iOrderUploadRepository.save(orderUpload);

        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    /**
     * @description: 工程师完成维修
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:23
     **/
    @Override
    @Transactional
    public void finishRepair(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }


        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        // 检查是否完成文件上传
        List<OrderUpload> orderUploads = iOrderUploadRepository.lambdaQuery().eq(OrderUpload::getOrderId, orderId).eq(OrderUpload::getUploaderType, OrderUploaderTypeEnum.ENGINEER).eq(OrderUpload::getFileType, OrderUploadFileTypeEnum.VIDEO).list();

        if (CollUtil.isEmpty(orderUploads) || orderUploads.size() != 1) {
            log.error("工程师上传文件数量不符合要求！");
            throw new GracefulResponseException("工程师未上传或未按要求上传视频文件，请联系系统管理员！");
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RECHECK, order.getOrderId())))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 工程师返还物品
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:23
     **/
    @Override
    @Transactional
    public void returnItem(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getEngineerLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RETURN, order.getOrderId())))) {
            throw new IllegalOrderStatusFlowException();
        }
    }

    /**
     * @description: 客户关闭工单
     * @return:
     * @author: edoclin
     * @created: 2024/6/2 15:25
     **/
    @Override
    @Transactional
    public void closeOrder(String orderId, String loginId) {
        Order order = iOrderRepository.getById(orderId);
        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getClientLoginId(), loginId)) {
            throw new IllegalOrderLoginIdException();
        }

        if (Boolean.FALSE.equals(sendEvent(statusFlow(OrderStatusChangeEventEnum.CLIENT_CLOSED, order.getOrderId())))) {
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
            if (Boolean.TRUE.equals(redisTemplate.hasKey(NAMESPACE_4_MACHINE_STATE + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME)))) {
                // 存在持久化对象则恢复
                orderRedisPersister.restore(stateMachine, NAMESPACE_4_MACHINE_STATE + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
            }
            result = stateMachine.sendEvent(message);
            orderRedisPersister.persist(stateMachine, NAMESPACE_4_MACHINE_STATE + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
        } catch (Exception e) {
            throw new GracefulResponseException(e.getMessage());
        } finally {
            if (stateMachine != null) {
                // todo 工单结束，删除持久化，目前还存在问题！！！
                if (OrderStatusEnum.CLOSED.equals(stateMachine.getState().getId())) {
                    redisTemplate.delete(NAMESPACE_4_MACHINE_STATE + message.getHeaders().get(STATE_MACHINE_HEADER_ORDER_NAME));
                }
                stateMachine.stop();
            }
        }
        return result;
    }
}
