package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.OrderMaterialGroupVo;
import com.mi.aftersales.service.OrderMaterialService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 工单评价 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/order-material")
public class OrderMaterialController {
    @Resource
    private OrderMaterialService orderMaterialService;



    @PostMapping(path = "/manager/applying")
    @Operation(summary = "查看申请中的物料", description = "查看申请中的物料")
    @CheckLogin
    public PageResult<OrderMaterialGroupVo> listApplyingOrderMaterial(@RequestBody @Valid ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        return orderMaterialService.listOrderMaterialGroupByOrder(query);
    }
}
