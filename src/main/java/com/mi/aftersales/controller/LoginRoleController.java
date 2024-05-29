package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import com.mi.aftersales.service.ILoginRoleService;
import com.mi.aftersales.vo.form.LoginRoleForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 用户角色信息表 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
@RestController
@RequestMapping("/aftersales/loginRole")
public class LoginRoleController {
    @Resource
    private ILoginRoleService iLoginRoleService;


    @PostMapping(path = "/")
    @Operation(summary = "添加/修改用户角色", description = "添加修改用户角色")
    @CheckLogin
    public void postLoginRole(@RequestBody @Valid LoginRoleForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        iLoginRoleService.addOrUpdateLoginRole(form);
    }
}
