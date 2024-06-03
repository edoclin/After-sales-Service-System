package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.MiddleLoginPermission;
import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.MiddleLoginPermissionService;
import com.mi.aftersales.repository.ILoginRepository;
import com.mi.aftersales.repository.IMiddleLoginPermissionRepository;
import com.mi.aftersales.repository.IPermissionRepository;
import com.mi.aftersales.pojo.vo.form.LoginPermissionFormVo;
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
public class MiddleLoginPermissionServiceImpl implements MiddleLoginPermissionService {
    @Resource
    private ILoginRepository iLoginRepository;

    @Resource
    private IPermissionRepository iPermissionRepository;

    @Resource
    private IMiddleLoginPermissionRepository iMiddleLoginPermissionRepository;

    @Override
    public void addLoginPermission(LoginPermissionFormVo form) {
        try {
            Login login = iLoginRepository.getById(form.getLoginId());
            if (BeanUtil.isEmpty(login)) {
                throw new GracefulResponseException("用户不存在");
            }

            Permission permission = iPermissionRepository.getById(form.getPermissionId());
            if (BeanUtil.isEmpty(permission)) {
                throw new GracefulResponseException("权限不存在");
            }

            MiddleLoginPermission save = new MiddleLoginPermission();
            save.setLoginId(login.getLoginId());
            save.setPermissionId(permission.getPermissionId());
            iMiddleLoginPermissionRepository.save(save);
        } catch (Exception e) {
            throw new ServerErrorException();
        }
    }
}
