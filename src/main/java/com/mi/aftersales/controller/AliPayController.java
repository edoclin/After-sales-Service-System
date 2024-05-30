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
import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.entity.enums.PayMethodEnum;
import com.mi.aftersales.entity.enums.PayStatusEnum;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderLoginIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderStatusFlowException;
import com.mi.aftersales.service.AliPayService;
import com.mi.aftersales.service.OrderService;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.repository.IPayOrderRepository;
import com.mi.aftersales.vo.PayDetailVo;
import com.mi.aftersales.vo.result.AlipayResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
    private AliPayService aliPayService;


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
        return aliPayService.alipay(orderId);
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
    public void returnCallback(String out_trade_no) {
        // todo 未进行过期处理
        aliPayService.returnCallback(out_trade_no);
    }
}
