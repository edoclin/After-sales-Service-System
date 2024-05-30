package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.repository.IPayOrderRepository;
import com.mi.aftersales.service.PayOrderService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.PayOrderResult;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 工单支付记录 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class PayOrderServiceImpl implements PayOrderService {

    @Resource
    private IOrderRepository iOrderRepository;

    @Resource
    private IPayOrderRepository iPayOrderRepository;

    @Override
    public List<PayOrderResult> listClientPayOrders() {
        ArrayList<PayOrderResult> result = new ArrayList<>();

        iOrderRepository.lambdaQuery().eq(Order::getCreatedId, StpUtil.getLoginIdAsString()).list().forEach(order -> {
            iPayOrderRepository.lambdaQuery().eq(PayOrder::getOrderId, order.getOrderId()).list().forEach(payOrder -> {
                PayOrderResult payOrderResult = new PayOrderResult();
                BeanUtil.copyProperties(payOrder, payOrderResult, DateUtil.copyDate2yyyyMMddHHmm());
                payOrderResult.setPayStatus(payOrder.getPayStatus().getDesc());
                payOrderResult.setPayMethod(payOrder.getPayMethod().getDesc());

                result.add(payOrderResult);
            });
        });

        return result;
    }

    @Override
    public PageResult<PayOrderResult> listClientOrderByCondition(ConditionQuery query){
        PageResult<PayOrderResult> result = new PageResult<>();
        QueryWrapper<PayOrder> wrapper = QueryUtil.buildWrapper(query, PayOrder.class);
        result.setTotal(iPayOrderRepository.count(wrapper));
        iPayOrderRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(payOrder -> {
            PayOrderResult payOrderResult = new PayOrderResult();
            BeanUtil.copyProperties(payOrder, payOrderResult, DateUtil.copyDate2yyyyMMddHHmm());
            payOrderResult.setPayStatus(payOrder.getPayStatus().getDesc());
            payOrderResult.setPayMethod(payOrder.getPayMethod().getDesc());

            result.getData().add(payOrderResult);
        });
        return result;
    }

}
