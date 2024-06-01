package com.mi.aftersales.controller;

import cn.dev33.satoken.SaManager;
import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.enums.*;
import com.mi.aftersales.service.OrderService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.form.*;
import com.mi.aftersales.vo.result.*;
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

    @GetMapping(path = "/client/{orderId}")
    @Operation(summary = "客户查询工单详情", description = "客户查询工单详情")
    public ClientOrderDetailVo orderDetailById(@PathVariable String orderId) {
        return orderService.getClientOrderDetail(orderId, StpUtil.getLoginIdAsString());
    }

    @PostMapping(path = "/client/create")
    @Operation(summary = "客户创建工单", description = "客户创建工单")
    @CheckLogin
    public void postOrder(@RequestBody @Valid ClientOrderForm form) {
        orderService.createOrder(form, StpUtil.getLoginIdAsString());
    }

    @GetMapping(path = "/engineer/pending/{spuCategoryId}")
    @Operation(summary = "工程师查询待办工单", description = "工程师查询待办工单")
    @CheckLogin
    public List<EngineerSimpleOrderVo> listPendingOrder(@PathVariable Integer spuCategoryId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        return orderService.listPendingOrders(spuCategoryId);
    }

    @GetMapping(path = "/engineer/accept/{orderId}")
    @Operation(summary = "工程师接受工单", description = "工程师接受工单")
    @CheckLogin
    @Transactional
    public void engineerAcceptOrder(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.acceptOrder(orderId, StpUtil.getLoginIdAsString());
    }

    @PostMapping(path = "/engineer/upload/before")
    @Operation(summary = "工程师上传检测前图片", description = "工程师上传检测前图片")
    @CheckLogin
    public void engineerAcceptOrder(@RequestBody @Valid EngineerUploadForm form) {
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

    @PutMapping(path = "/engineer/faultDesc")
    @Operation(summary = "工程师上传故障描述", description = "工程师拆机维修，工程师故障描述")
    @CheckLogin
    public void faultDescription(@RequestBody @Valid FaultDescriptionForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.uploadFaultDescription(form, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/feeConfirm")
    @Operation(summary = "工程师确认计费", description = "工程师确认计费")
    @CheckLogin
    public void engineerFeeConfirm(@RequestBody @Valid OrderFeeConfirmForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.confirmFee(form, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client/feeConfirm/{orderId}")
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

    @PutMapping(path = "/material/apply/{orderId}")
    @Operation(summary = "工程师申请物料", description = "工程师申请物料")
    @CheckLogin
    public void materialApply(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.applyMaterial(orderId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/material/distribute")
    @Operation(summary = "库管处理请求（分发物料）", description = "库管处理请求（分发物料）")
    @CheckLogin
    public void materialDistribute(@RequestBody @Valid MaterialDistributeForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        orderService.distributeMaterial(form, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/engineer/repair/{orderId}/{material}")
    @Operation(summary = "工程师开始维修", description = "工程师开始维修")
    @Parameter(name = "material", description = "无需物料：false")
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
    public void engineerUploadVideo(@RequestBody @Valid EngineerUploadForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.engineerUploadVideo(form);
    }

    @PutMapping(path = "/engineer/toBePaid/{orderId}")
    @Operation(summary = "工程师完成维修，发送账单，等待支付", description = "工程师完成维修，发送账单，等待支付")
    @CheckLogin
    public void engineerFinishedRepair(@PathVariable String orderId) {
        StpUtil.checkRole(EmployeeRoleEnum.ENGINEER.name());
        orderService.finishRepair(orderId, StpUtil.getLoginIdAsString());
    }

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
