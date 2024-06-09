package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.pojo.vo.PayOrderVo;
import com.mi.aftersales.pojo.vo.PermissionVo;
import com.mi.aftersales.pojo.vo.form.PermissionApiFormVo;
import com.mi.aftersales.pojo.vo.form.UpdatePermissionApiFormVo;
import com.mi.aftersales.service.PermissionService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import org.apache.ibatis.annotations.Delete;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 权限 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/permission")
public class PermissionController {

    @Resource
    private PermissionService permissionService;

    @GetMapping(path = "/manager")
    @Operation(summary = "查询所有权限（SYSTEM_MANAGER）", description = "查询所有权限（SYSTEM_MANAGER）")
    @CheckLogin
    public PageResult<PermissionVo> listClientOrder(@RequestBody @Valid ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        return permissionService.listPermission(query);
    }

    @PostMapping(path = "/manager")
    @Operation(summary = "添加权限（SYSTEM_MANAGER）", description = "添加权限（SYSTEM_MANAGER）")
    @CheckLogin
    public void postPermission(@RequestBody @Valid PermissionApiFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        permissionService.addPermission(form);
    }

    @DeleteMapping(path = "/manager/{permissionId}")
    @Operation(summary = "删除权限（SYSTEM_MANAGER）", description = "添加权限（SYSTEM_MANAGER）")
    @CheckLogin
    public void postPermission(@PathVariable String permissionId) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        permissionService.removePermissionById(permissionId);
    }


    @PutMapping(path = "/manager")
    @Operation(summary = "修改权限（SYSTEM_MANAGER）", description = "修改权限（SYSTEM_MANAGER）")
    @CheckLogin
    public void putPermission(@RequestBody @Valid UpdatePermissionApiFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        permissionService.updatePermissionById(form);
    }
}
