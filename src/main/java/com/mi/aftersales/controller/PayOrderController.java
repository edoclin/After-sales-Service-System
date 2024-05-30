package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.PayOrderService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.PayOrderResult;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * <p>
 * 工单支付记录 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/payOrder")
public class PayOrderController {

    @Resource
    private PayOrderService payOrderService;

    @PostMapping(path = "/client")
    @Operation(summary = "查询当前用户的支付账单", description = "查询当前用户的支付账单")
    @CheckLogin
    public List<PayOrderResult> listClientOrder() {
        return payOrderService.listClientPayOrders();
    }

    @PostMapping(path = "/client")
    @Operation(summary = "客户查询工单", description = "客户查询工单")
    public PageResult<PayOrderResult>  listClientOrderByCondition(@RequestBody ConditionQuery query) {
        return payOrderService.listClientOrderByCondition(query);
    }
}
