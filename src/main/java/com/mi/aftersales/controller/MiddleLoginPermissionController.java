package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.anno.CheckPermission;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.MiddleLoginPermission;
import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.service.ILoginService;
import com.mi.aftersales.service.IMiddleLoginPermissionService;
import com.mi.aftersales.service.IPermissionService;
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
    private ILoginService iLoginService;

    @Resource
    private IMiddleLoginPermissionService iMiddleLoginPermissionService;

    @Resource
    private IPermissionService iPermissionService;

    @PostMapping(path = "/")
    @CheckPermission
    @Operation(summary = "关联用户权限", description = "关联用户权限")
    public void postMiddleLoginPermission(@RequestBody @Valid LoginPermissionForm form) {
        Login login = iLoginService.getById(form.getLoginId());
        if (BeanUtil.isEmpty(login)) {
            throw new GracefulResponseException("用户不存在");
        }

        Permission permission = iPermissionService.getById(form.getPermissionId());

        if (BeanUtil.isEmpty(permission)) {
            throw new GracefulResponseException("权限不存在");
        }

        MiddleLoginPermission save = new MiddleLoginPermission();
        save.setLoginId(login.getLoginId());
        save.setPermissionId(permission.getPermissionId());
        iMiddleLoginPermissionService.save(save);
    }
}
