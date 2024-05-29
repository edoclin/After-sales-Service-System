package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.service.OrderStatusLogService;
import com.mi.aftersales.service.iservice.IOrderService;
import com.mi.aftersales.service.iservice.IOrderStatusLogService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.OrderStatusLogResult;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 工单状态日志 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class OrderStatusLogServiceImpl implements OrderStatusLogService {
    @Resource
    private IOrderStatusLogService iOrderStatusLogService;

    @Resource
    private IOrderService iOrderService;

    @Override
    public List<OrderStatusLogResult> listOrderStatusLogByOrderId(String orderId) {
        if (BeanUtil.isEmpty(iOrderService.getById(orderId))) {
            throw new IllegalOrderIdException();
        }
        List<OrderStatusLogResult> result = new ArrayList<>();
        iOrderStatusLogService.list(new QueryWrapper<OrderStatusLog>().eq("order_id", orderId)).forEach(statusLog -> {
            OrderStatusLogResult item = new OrderStatusLogResult();
            BeanUtil.copyProperties(statusLog, item, DateUtil.copyDate2yyyyMMddHHmm());
            item.setOrderStatus(statusLog.getOrderStatus().getDesc());
            result.add(item);
        });
        return result;
    }
}
