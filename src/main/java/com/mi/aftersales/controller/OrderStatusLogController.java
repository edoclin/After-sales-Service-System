package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.OrderStatusLogService;
import com.mi.aftersales.pojo.vo.OrderStatusLogVo;
import io.swagger.v3.oas.annotations.Operation;

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
    private OrderStatusLogService orderStatusLogService;

    @GetMapping(path = "/{orderId}")
    @Operation(summary = "查询工单状态日志", description = "查询工单状态日志")
    @CheckLogin
    public List<OrderStatusLogVo> listOrderStatusLogById(@PathVariable String orderId) {
        return orderStatusLogService.listOrderStatusLogByOrderId(orderId);
    }
}
