package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.IOrderService;
import com.mi.aftersales.service.IOrderStatusLogService;
import com.mi.aftersales.vo.OrderStatusLogResult;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
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

    @GetMapping(path = "/{orderId}")
    @Operation(summary = "查询工单状态日志", description = "查询工单状态日志")
    @CheckLogin
    public List<OrderStatusLogResult> listOrderStatusLogById(@PathVariable String orderId) {
        return iOrderStatusLogService.listOrderStatusLogByOrderId(orderId);
    }
}
