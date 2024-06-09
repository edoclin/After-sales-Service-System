package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.LoginPermission;
import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.exception.graceful.BaseCustomException;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.IllegalPermissionIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.pojo.vo.form.LoginPermissionFormVo;
import com.mi.aftersales.repository.ILoginRepository;
import com.mi.aftersales.repository.ILoginPermissionRepository;
import com.mi.aftersales.repository.IPermissionRepository;
import com.mi.aftersales.service.LoginPermissionService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <p>
 * 用户具有权限中间表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class LoginPermissionServiceImpl implements LoginPermissionService {
    @Resource
    private ILoginRepository iLoginRepository;

    @Resource
    private IPermissionRepository iPermissionRepository;

    @Resource
    private ILoginPermissionRepository iLoginPermissionRepository;

    @Override
    public void addLoginPermission(LoginPermissionFormVo form) {
        try {
            Login login = iLoginRepository.getById(form.getLoginId());
            if (BeanUtil.isEmpty(login)) {
                throw new IllegalLoginIdException();
            }

            Permission permission = iPermissionRepository.getById(form.getPermissionId());
            if (BeanUtil.isEmpty(permission)) {
                throw new IllegalPermissionIdException();
            }

            LoginPermission save = new LoginPermission();
            save.setLoginId(login.getLoginId());
            save.setPermissionId(permission.getPermissionId());
            iLoginPermissionRepository.save(save);
        } catch (BaseCustomException e) {
            throw e;
        } catch (Exception e) {
            throw new ServerErrorException();
        }
    }
}
