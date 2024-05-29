package com.mi.aftersales.service.iservice.impl;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.MiddleLoginPermission;
import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mapper.LoginMapper;
import com.mi.aftersales.mapper.MiddleLoginPermissionMapper;
import com.mi.aftersales.service.iservice.IMiddleLoginPermissionService;
import com.mi.aftersales.mapper.PermissionMapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.vo.form.LoginPermissionForm;
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
public class MiddleLoginPermissionServiceImpl extends ServiceImpl<MiddleLoginPermissionMapper, MiddleLoginPermission> implements IMiddleLoginPermissionService {
    @Resource
    private LoginMapper loginMapper;

    @Resource
    private PermissionMapper permissionMapper;

    @Resource
    private MiddleLoginPermissionMapper middleLoginPermissionMapper;

    @Override
    public void addLoginPermission(LoginPermissionForm form) {
        try {
            Login login = loginMapper.selectById(form.getLoginId());
            if (BeanUtil.isEmpty(login)) {
                throw new GracefulResponseException("用户不存在");
            }

            Permission permission = permissionMapper.selectById(form.getPermissionId());
            if (BeanUtil.isEmpty(permission)) {
                throw new GracefulResponseException("权限不存在");
            }

            MiddleLoginPermission save = new MiddleLoginPermission();
            save.setLoginId(login.getLoginId());
            save.setPermissionId(permission.getPermissionId());
            middleLoginPermissionMapper.insert(save);
        } catch (Exception e) {
            throw new ServerErrorException();
        }
    }
}
