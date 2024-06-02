package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.LoginRoleService;
import com.mi.aftersales.repository.ILoginRoleRepository;
import com.mi.aftersales.repository.ILoginRepository;
import com.mi.aftersales.pojo.vo.form.LoginRoleFormVo;
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
    private ILoginRepository iLoginRepository;

    @Resource
    private ILoginRoleRepository iLoginRoleRepository;

    @Override
    public void addOrUpdateLoginRole(LoginRoleFormVo form) {
        Login login = iLoginRepository.getById(form.getLoginId());

        if (BeanUtil.isEmpty(login)) {
            throw new IllegalLoginIdException();
        }
        iLoginRoleRepository.remove(Wrappers.lambdaQuery(LoginRole.class).eq(LoginRole::getLoginId, form.getLoginId()));
        for (EmployeeRoleEnum role : form.getRoles()) {
            LoginRole one = new LoginRole();
            one.setLoginId(login.getLoginId());
            one.setEmployeeRole(role);
            try {
                iLoginRoleRepository.save(one);
            } catch (Exception e) {
                throw new ServerErrorException();
            }
        }
    }
}
