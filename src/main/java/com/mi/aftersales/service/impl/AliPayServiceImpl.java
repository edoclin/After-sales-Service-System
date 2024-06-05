package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import cn.hutool.json.JSONUtil;
import com.alipay.api.AlipayApiException;
import com.alipay.api.AlipayClient;
import com.alipay.api.DefaultAlipayClient;
import com.alipay.api.domain.AlipayTradePagePayModel;
import com.alipay.api.request.AlipayTradePagePayRequest;
import com.alipay.api.response.AlipayTradePagePayResponse;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.yaml.bean.AliPayConfig;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import com.mi.aftersales.enums.entity.PayMethodEnum;
import com.mi.aftersales.enums.entity.PayStatusEnum;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderLoginIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderStatusFlowException;
import com.mi.aftersales.exception.graceful.IllegalPayOrderIdException;
import com.mi.aftersales.mq.producer.MqProducer;
import com.mi.aftersales.pojo.vo.AlipayVo;
import com.mi.aftersales.pojo.vo.PayDetailVo;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.repository.IPayOrderRepository;
import com.mi.aftersales.service.AliPayService;
import com.mi.aftersales.service.OrderService;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ALIPAY_ORDER;
import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_LOG;

/**
 * @description: 支付宝支付服务
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 12:30
 **/
@Service
public class AliPayServiceImpl implements AliPayService {

    @Resource
    private AliPayConfig aliPayConfig;

    @Resource
    private IOrderRepository iOrderRepository;
    @Resource
    private OrderService orderService;

    @Resource
    private MqProducer mqProducer;

    @Resource
    private IPayOrderRepository iPayOrderRepository;


    /**
     * @description: 支付宝订单生成
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @Override
    public AlipayVo alipay(@PathVariable String orderId) throws AlipayApiException {
        Order order = iOrderRepository.getById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new IllegalOrderIdException();
        }

        if (!CharSequenceUtil.equals(order.getClientLoginId(), StpUtil.getLoginIdAsString())) {
            throw new IllegalOrderLoginIdException();
        }

        if (!OrderStatusEnum.TO_BE_PAID.equals(order.getOrderStatus() )) {
            throw new GracefulResponseException("该工单尚未到达支付流程！");
        }

        AlipayClient alipayClient = new DefaultAlipayClient(aliPayConfig.getAlipayGatewayUrl(), aliPayConfig.getAppId(), aliPayConfig.getPrivateKey(), "json", "utf-8", aliPayConfig.getPublicKey(), aliPayConfig.getSignType());

        AlipayTradePagePayRequest request = new AlipayTradePagePayRequest();
        request.setReturnUrl(aliPayConfig.getReturnUrl());
        AlipayTradePagePayModel model = new AlipayTradePagePayModel();

        PayOrder payOrder = new PayOrder();

        payOrder.setPayOrderId(IdUtil.getSnowflakeNextIdStr());


        model.setOutTradeNo(payOrder.getPayOrderId());

        BigDecimal amount = order.getManualFee().add(order.getMaterialFee());
        model.setTotalAmount(String.valueOf(amount));
        model.setSubject("支付宝沙箱支付");
        model.setProductCode("FAST_INSTANT_TRADE_PAY");
        request.setBizModel(model);
        AlipayTradePagePayResponse response = alipayClient.pageExecute(request);
        AlipayVo result = new AlipayVo();
        result.setBody(response.getBody());


        payOrder
                .setPayMethod(PayMethodEnum.ALIPAY)
                .setAmount(amount)
                .setPayStatus(PayStatusEnum.WAITING)
                .setOrderId(orderId)
                .setCreatedId(StpUtil.getLoginIdAsString());

        // 异步创建订单
        mqProducer.asyncSend(ROCKETMQ_TOPIC_4_ALIPAY_ORDER, payOrder);
        return result;
    }

    /**
     * @description: 同步回调接口, 支付结果回调. 生产环境使用支付宝异步回调.
     * 同步回调无法判断是否成功，默认支付成功
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @Override
    public void returnCallback(String out_trade_no) {
        // 同步回调 模拟支付成功
        // outTradeNo 对应payOrderId
        PayOrder payOrder = iPayOrderRepository.getById(out_trade_no);

        if (BeanUtil.isEmpty(payOrder)) {
            throw new IllegalPayOrderIdException();
        }

        payOrder.setPayStatus(PayStatusEnum.PAID);
        payOrder.setPayDetail(JSONUtil.toJsonStr(new PayDetailVo().setPaidTime(LocalDateTime.now())));

        if (!orderService.sendEvent(orderService.statusFlow(OrderStatusChangeEventEnum.CLIENT_COMPLETED_PAY, payOrder.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }


        OrderStatusLog orderStatusLog = new OrderStatusLog();
        orderStatusLog.setOrderId(payOrder.getOrderId()).setOrderStatus(OrderStatusEnum.PAID);
        orderStatusLog.setStatusDetail("客户完成支付，工程师准备返还维修物品！");


        mqProducer.asyncSend(ROCKETMQ_TOPIC_4_ORDER_LOG, orderStatusLog);

        orderService.sendSms(payOrder.getOrderId());

        // 更新支付订单
        mqProducer.asyncSend(ROCKETMQ_TOPIC_4_ALIPAY_ORDER, payOrder);
    }

}
