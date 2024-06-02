package com.mi.aftersales.service;

import com.alipay.api.AlipayApiException;
import com.mi.aftersales.pojo.vo.AlipayVo;
import org.springframework.web.bind.annotation.PathVariable;

public interface AliPayService {
    AlipayVo alipay(@PathVariable String orderId) throws AlipayApiException;

    void returnCallback(String out_trade_no);
}
