package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.NotRoleException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.ILoginRoleService;
import com.mi.aftersales.service.ILoginService;
import com.mi.aftersales.vo.form.LoginRoleForm;
import com.mi.aftersales.vo.form.MaterialDistributeForm;
import io.swagger.v3.oas.annotations.Operation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;
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
@RequestMapping("/aftersales/loginRole")
public class LoginRoleController {

    private static final Logger log = LoggerFactory.getLogger(LoginRoleController.class);
    @Resource
    private ILoginService iLoginService;

    @Resource
    private ILoginRoleService iLoginRoleService;


    @PostMapping(path = "/")
    @Operation(summary = "添加/修改用户角色", description = "添加修改用户角色")
    @CheckLogin
    @Transactional
    public void postLoginRole(@RequestBody @Valid LoginRoleForm form) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        Login login = iLoginService.getById(form.getLoginId());

        if (BeanUtil.isEmpty(login)) {
            throw new IllegalLoginIdException();
        }
        iLoginRoleService.remove(Wrappers.lambdaQuery(LoginRole.class).eq(LoginRole::getLoginId, form.getLoginId()));
        for (EmployeeRoleEnum role : form.getRoles()) {
            LoginRole one = new LoginRole();
            one.setLoginId(login.getLoginId());
            one.setEmployeeRole(role);
            try {
                iLoginRoleService.save(one);
            } catch (Exception e) {
                log.error(e.getMessage());
                throw new ServerErrorException();
            }
        }
    }
}
