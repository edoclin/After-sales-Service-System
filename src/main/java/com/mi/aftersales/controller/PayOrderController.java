package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.PayOrderVo;
import com.mi.aftersales.service.PayOrderService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

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
@RequestMapping("/aftersales/pay-order")
public class PayOrderController {

    @Resource
    private PayOrderService payOrderService;

    @GetMapping(path = "/client")
    @Operation(summary = "查询当前用户的支付账单", description = "查询当前用户的支付账单")
    @CheckLogin
    public List<PayOrderVo> listClientOrder() {
        return payOrderService.listClientPayOrders();
    }

    @PostMapping(path = "/manager")
    @Operation(summary = "查询支付账单", description = "查询支付账单")
    @CheckLogin
    public PageResult<PayOrderVo> listClientOrderByCondition(@RequestBody ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        return payOrderService.listClientOrderByCondition(query);
    }
}
