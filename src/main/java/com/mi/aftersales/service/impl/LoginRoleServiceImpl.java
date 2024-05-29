package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mapper.LoginMapper;
import com.mi.aftersales.mapper.LoginRoleMapper;
import com.mi.aftersales.service.ILoginRoleService;
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
public class LoginRoleServiceImpl extends ServiceImpl<LoginRoleMapper, LoginRole> implements ILoginRoleService {
    @Resource
    private LoginMapper loginMapper;

    @Resource
    private LoginRoleMapper loginRoleMapper;

    @Override
    public void addOrUpdateLoginRole(LoginRoleForm form) {
        Login login = loginMapper.selectById(form.getLoginId());

        if (BeanUtil.isEmpty(login)) {
            throw new IllegalLoginIdException();
        }

        loginRoleMapper.delete(Wrappers.lambdaQuery(LoginRole.class).eq(LoginRole::getLoginId, form.getLoginId()));

        for (EmployeeRoleEnum role : form.getRoles()) {
            LoginRole one = new LoginRole();
            one.setLoginId(login.getLoginId());
            one.setEmployeeRole(role);
            try {
                loginRoleMapper.insert(one);
            } catch (Exception e) {
                throw new ServerErrorException(e.getMessage());
            }
        }
    }
}
