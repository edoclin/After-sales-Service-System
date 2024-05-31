package com.mi.aftersales.service;

import com.alipay.api.AlipayApiException;
import com.mi.aftersales.vo.result.AlipayResult;
import org.springframework.web.bind.annotation.PathVariable;

public interface AliPayService {
    AlipayResult alipay(@PathVariable String orderId) throws AlipayApiException;

    void returnCallback(String out_trade_no);
}
