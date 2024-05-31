package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.PayOrderResult;

import java.util.List;

/**
 * <p>
 * 工单支付记录 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface PayOrderService {

    List<PayOrderResult> listClientPayOrders();

    PageResult<PayOrderResult> listClientOrderByCondition(ConditionQuery query);
}
