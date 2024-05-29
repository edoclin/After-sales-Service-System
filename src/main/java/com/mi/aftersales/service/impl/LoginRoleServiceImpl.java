package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.LoginRoleService;
import com.mi.aftersales.service.iservice.ILoginRoleService;
import com.mi.aftersales.service.iservice.ILoginService;
import com.mi.aftersales.vo.form.LoginRoleForm;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
/**
 * <p>
 * 员工信息表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
@Service
public class LoginRoleServiceImpl implements LoginRoleService {
    @Resource
    private ILoginService iLoginService;

    @Resource
    private ILoginRoleService iLoginRoleService;

    @Override
    public void addOrUpdateLoginRole(LoginRoleForm form) {
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
                throw new ServerErrorException();
            }
        }
    }
}
