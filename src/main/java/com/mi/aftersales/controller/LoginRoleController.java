package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.LoginRoleService;
import com.mi.aftersales.pojo.vo.form.LoginRoleFormVo;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 员工信息表 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
@RestController
@RequestMapping("/aftersales/login-role")
public class LoginRoleController {
    @Resource
    private LoginRoleService loginRoleService;

    @PostMapping(path = "/")
    @Operation(summary = "添加/修改用户角色", description = "添加修改用户角色")
    @CheckLogin
    public void postLoginRole(@RequestBody @Valid LoginRoleFormVo form) {
        loginRoleService.addOrUpdateLoginRole(form);
    }
}
