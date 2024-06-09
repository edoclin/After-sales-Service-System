package com.mi.aftersales.controller;

import com.mi.aftersales.pojo.vo.form.LoginPermissionFormVo;
import com.mi.aftersales.service.LoginPermissionService;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 用户具有权限中间表 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/login-permission")
public class LoginPermissionController {
    @Resource
    private LoginPermissionService loginPermissionService;

    @PostMapping(path = "/manager")
    @Operation(summary = "关联用户权限", description = "关联用户权限")
    public void postMiddleLoginPermission(@RequestBody @Valid LoginPermissionFormVo form) {
        loginPermissionService.addLoginPermission(form);
    }
}
