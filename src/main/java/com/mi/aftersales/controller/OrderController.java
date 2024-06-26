package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.exception.graceful.TransactionalErrorException;
import com.mi.aftersales.pojo.vo.ClientOrderSimpleVo;
import com.mi.aftersales.pojo.vo.OrderDetailVo;
import com.mi.aftersales.pojo.vo.OrderSimple4EngineerVo;
import com.mi.aftersales.pojo.vo.PendingOrderSimple4EngineerVo;
import com.mi.aftersales.pojo.vo.form.*;
import com.mi.aftersales.service.OrderService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 工单 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/order")
public class OrderController {
    @Resource
    private OrderService orderService;

    @PostMapping(path = "/client")
    @Operation(summary = "客户查询工单", description = "客户查询工单")
    public List<ClientOrderSimpleVo> listClientOrder(@RequestBody ConditionQuery query) {
        return orderService.listClientOrders(query, StpUtil.getLoginIdAsString());
    }

    @PostMapping(path = "/engineer")
    @Operation(summary = "工程师查询所属工单", description = "工程师查询所属工单")
    public PageResult<OrderSimple4EngineerVo> listEngineerOrder(@RequestBody ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        return orderService.listEngineerOrder(query);
    }

    @GetMapping(path = "/client/detail/{orderId}")
    @Operation(summary = "客户查询工单详情", description = "客户查询工单详情")
    public OrderDetailVo orderDetailByClientId(@PathVariable String orderId) {
        return orderService.getClientOrderDetail(orderId, StpUtil.getLoginIdAsString(), Boolean.TRUE);
    }

    @GetMapping(path = "/engineer/detail/{orderId}")
    @Operation(summary = "工程师查询工单详情", description = "工程师查询工单详情")
    public OrderDetailVo orderDetailByEngineerId(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        return orderService.getClientOrderDetail(orderId, StpUtil.getLoginIdAsString(), Boolean.FALSE);
    }

    @PostMapping(path = "/client/create")
    @Operation(summary = "客户创建工单", description = "客户创建工单")
    @CheckLogin
    public void postOrder(@RequestBody @Valid ClientOrderFormVo form) {
        orderService.createOrder(form, StpUtil.getLoginIdAsString());
    }

    @GetMapping(path = "/engineer/pending/{spuCategoryId}")
    @Operation(summary = "工程师查询待办工单", description = "工程师查询待办工单")
    @CheckLogin
    public List<PendingOrderSimple4EngineerVo> listPendingOrder(@PathVariable Integer spuCategoryId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        return orderService.listPendingOrders(spuCategoryId);
    }

    @GetMapping(path = "/engineer/accept/{orderId}")
    @Operation(summary = "工程师接受工单", description = "工程师接受工单")
    @CheckLogin
    @Transactional(rollbackFor = TransactionalErrorException.class)
    public void engineerAcceptOrder(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.acceptOrder(orderId, StpUtil.getLoginIdAsString());
    }

    @PostMapping(path = "/engineer/upload/before")
    @Operation(summary = "工程师上传检测前图片", description = "工程师上传检测前图片")
    @CheckLogin
    public void engineerAcceptOrder(@RequestBody @Valid EngineerUploadFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.engineerUploadImage(form);
    }

    @PutMapping(path = "/engineer/checking/{orderId}")
    @Operation(summary = "工程师开始检测", description = "工程师开始检测")
    @CheckLogin
    public void startRepairMachine(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.startChecking(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/fault-desc")
    @Operation(summary = "工程师上传故障描述", description = "工程师拆机维修，工程师故障描述")
    @CheckLogin
    public void faultDescription(@RequestBody @Valid FaultDescriptionFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.uploadFaultDescription(form, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/fee-confirm")
    @Operation(summary = "工程师确认计费", description = "工程师确认计费")
    @CheckLogin
    public void engineerFeeConfirm(@RequestBody @Valid OrderFeeConfirmFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.confirmFee(form, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client/fee-confirm/{orderId}")
    @Operation(summary = "用户确认计费（确认维修）", description = "用户确认计费（确认维修）")
    @CheckLogin
    public void clientConfirmFee(@PathVariable String orderId) {
        orderService.clientConfirmFee(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client/reject/{orderId}")
    @Operation(summary = "用户拒绝维修（返回物品）", description = "用户拒绝维修（返回物品）")
    @CheckLogin
    public void clientRejectRepair(@PathVariable String orderId) {
        orderService.clientRejectRepair(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/material-apply/{orderId}")
    @Operation(summary = "工程师申请物料", description = "工程师申请物料")
    @CheckLogin
    public void materialApply(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.applyMaterial(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/manager/material-distribute/{orderId}")
    @Operation(summary = "库管处理请求（分发物料）", description = "库管处理请求（分发物料）")
    @CheckLogin
    public void materialDistribute(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        orderService.distributeMaterial(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/repair/{orderId}/{material}")
    @Operation(summary = "工程师开始维修", description = "工程师开始维修")
    @Parameter(name = "material", description = "确认计费后直接开始维修时取false，确认维修后走申请物料为true")
    @CheckLogin
    public void engineerStartRepairing(@PathVariable String orderId, @PathVariable Boolean material) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.startRepair(orderId, material, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/recheck/{orderId}")
    @Operation(summary = "工程师开始复检", description = "工程师开始复检")
    @CheckLogin
    public void engineerStartRechecking(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.startRechecking(orderId, StpUtil.getLoginIdAsString());
    }


    @PutMapping(path = "/engineer/recheck/upload")
    @Operation(summary = "工程师上传维修结果（视频）", description = "工程师上传维修结果（视频）")
    @CheckLogin
    public void engineerUploadVideo(@RequestBody @Valid EngineerUploadFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.engineerUploadVideo(form);
    }

    @PutMapping(path = "/engineer/to-be-paid/{orderId}")
    @Operation(summary = "工程师完成复检，发送账单，等待支付", description = "工程师完成复检，发送账单，等待支付")
    @CheckLogin
    public void engineerFinishedRecheck(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.finishRecheck(orderId, StpUtil.getLoginIdAsString());
    }

    // 客户完成支付流程变更在支付回调中

    @PutMapping(path = "/engineer/return/{orderId}")
    @Operation(summary = "工程师开始返还物品", description = "工程师开始返还物品")
    @CheckLogin
    public void engineerReturn(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.returnItem(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client/close/{orderId}")
    @Operation(summary = "用户关闭工单", description = "用户关闭工单")
    @CheckLogin
    public void clientClose(@PathVariable String orderId) {
        orderService.closeOrder(orderId, StpUtil.getLoginIdAsString());
    }
}
