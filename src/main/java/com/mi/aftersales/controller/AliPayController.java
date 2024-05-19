package com.mi.aftersales.controller;

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
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.config.yaml.bean.AliPayConfig;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.entity.enums.PayMethodEnum;
import com.mi.aftersales.entity.enums.PayStatusEnum;
import com.mi.aftersales.exception.graceful.IllegalOrderStatusFlowException;
import com.mi.aftersales.service.IOrderService;
import com.mi.aftersales.service.IPayOrderService;
import com.mi.aftersales.vo.PayDetailVo;
import com.mi.aftersales.vo.result.AlipayResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.val;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ALIPAY_ORDER;

/**
 * @author edoclin
 */

@RestController
@RequestMapping("/aftersales/alipay")
class AliPayController {
    @Resource
    private AliPayConfig aliPayConfig;

    @Resource
    private IOrderService iOrderService;

    @Resource
    private RocketMQTemplate rocketmqTemplate;

    @Resource
    private IPayOrderService iPayOrderService;

    //todo 重构到service
    @Resource
    private OrderController orderController;

    /**
     * @description: 支付宝订单生成
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @GetMapping(path = "/pay/{orderId}")
    @CheckLogin
    @Operation(summary = "支付宝支付订单生成", description = "支付宝支付订单生成")
    public AlipayResult alipay(@PathVariable String orderId) throws AlipayApiException {
        Order order = iOrderService.getById(orderId);

        if (BeanUtil.isEmpty(order)) {
            throw new GracefulResponseException("订单号无效！");
        }

        if (!CharSequenceUtil.equals(order.getClientLoginId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("订单不属于当前用户！");
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
        AlipayResult result = new AlipayResult();
        result.setBody(response.getBody());


        payOrder
                .setPayMethod(PayMethodEnum.ALIPAY)
                .setAmount(amount)
                .setPayStatus(PayStatusEnum.WAITING)
                .setOrderId(orderId);

        // 异步创建订单
        Message<PayOrder> msg = MessageBuilder.withPayload(payOrder).build();
        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ALIPAY_ORDER, msg);
        return result;
    }

    /**
     * @description: 同步回调接口, 支付结果回调. 生产环境使用支付宝异步回调.
     * 同步回调无法判断是否成功，默认支付成功
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @GetMapping(path = "/return")
    @Operation(summary = "支付宝支付结果同步回调", description = "支付宝支付结果同步回调")
    @Parameter()
    public void returnCallback(String outTradeNo) {
        // 同步回调 模拟支付成功
        // outTradeNo 对应payOrderId
        PayOrder payOrder = iPayOrderService.getById(outTradeNo);

        if (BeanUtil.isEmpty(payOrder)) {
            throw new GracefulResponseException("支付订单Id非法！");
        }

        payOrder.setPayStatus(PayStatusEnum.PAID);
        payOrder.setPayDetail(JSONUtil.toJsonStr(new PayDetailVo().setPaidTime(LocalDateTime.now())));

        // 异步更新订单
        Message<PayOrder> msg = MessageBuilder.withPayload(payOrder).build();
        rocketmqTemplate.send(ROCKETMQ_TOPIC_4_ALIPAY_ORDER, msg);

        if (!orderController.sendEvent(OrderController.statusFlow(OrderStatusChangeEventEnum.CLIENT_COMPLETED_PAY, payOrder.getOrderId()))) {
            throw new IllegalOrderStatusFlowException();
        }
    }
}
