package com.mi.aftersales.controller;

import com.alipay.api.AlipayClient;
import com.alipay.api.DefaultAlipayClient;
import com.mi.aftersales.config.yaml.bean.AliPayConfig;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @author edoclin
 */

@RestController
@RequestMapping("/alipay")
class AliPayController {

    @Resource
    private AliPayConfig aliPayConfig;

    /**
     * @description: 支付宝订单生成
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @PostMapping(path = "/pay")
    @Operation(summary = "支付宝支付订单生成", description = "支付宝支付订单生成")
    public Object alipay() {
        // todo
        AlipayClient alipayClient = new DefaultAlipayClient(aliPayConfig.getAlipayGatewayUrl(), aliPayConfig.getAppId(), aliPayConfig.getPrivateKey(), "json", "utf-8", aliPayConfig.getPublicKey(), aliPayConfig.getSignType());
        return alipayClient;
    }

    /**
     * @description: 同步回调接口, 支付成功回调. 生产环境使用支付宝异步回调.
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:45
     **/
    @GetMapping(path = "/return")
    @Operation(summary = "支付宝支付结果同步回调", description = "支付宝支付结果同步回调")
    public Object returnCallback(String out_trade_no) {
        // 同步回调 模拟支付成功
        // todo
        return null;
    }
}
