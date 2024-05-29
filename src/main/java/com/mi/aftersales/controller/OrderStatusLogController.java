package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.service.iservice.IOrderService;
import com.mi.aftersales.service.iservice.IOrderStatusLogService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.OrderStatusLogResult;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 工单状态日志 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/orderStatusLog")
public class OrderStatusLogController {

    @Resource
    private IOrderStatusLogService iOrderStatusLogService;

    @Resource
    private IOrderService iOrderService;


    @GetMapping(path = "/{orderId}")
    @Operation(summary = "查询工单状态日志", description = "查询工单状态日志")
    @CheckLogin
    @Transactional
    public List<OrderStatusLogResult> listOrderStatusLogById(@PathVariable String orderId) {
        if (BeanUtil.isEmpty(iOrderService.getById(orderId))) {
            throw new IllegalOrderIdException();
        }
        List<OrderStatusLogResult> result = new ArrayList<>();
        iOrderStatusLogService.lambdaQuery().eq(OrderStatusLog::getOrderId, orderId).list().forEach(statusLog -> {
            OrderStatusLogResult item = new OrderStatusLogResult();
            BeanUtil.copyProperties(statusLog, item, DateUtil.copyDate2yyyyMMddHHmm());
            item.setOrderStatus(statusLog.getOrderStatus().getDesc());
            result.add(item);
        });
        return result;
    }
}
