package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckPermission;
import com.mi.aftersales.service.MiddleLoginPermissionService;
import com.mi.aftersales.vo.form.LoginPermissionForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

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
@RequestMapping("/aftersales/middleLoginPermission")
public class MiddleLoginPermissionController {
    @Resource
    private MiddleLoginPermissionService middleLoginPermissionService;

    @PostMapping(path = "/")
    @CheckPermission
    @Operation(summary = "关联用户权限", description = "关联用户权限")
    public void postMiddleLoginPermission(@RequestBody @Valid LoginPermissionForm form) {
        middleLoginPermissionService.addLoginPermission(form);
    }
}
