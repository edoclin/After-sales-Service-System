package com.mi.aftersales.controller;

import com.alipay.api.AlipayApiException;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.AliPayService;
import com.mi.aftersales.pojo.vo.AlipayVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

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
    public AlipayVo alipay(@PathVariable String orderId) throws AlipayApiException {
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
        aliPayService.returnCallback(out_trade_no);
    }
}
