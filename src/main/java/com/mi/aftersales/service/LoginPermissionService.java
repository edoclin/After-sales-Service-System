package com.mi.aftersales.service;

import com.mi.aftersales.pojo.vo.form.LoginPermissionFormVo;

/**
 * <p>
 * 用户具有权限中间表 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface LoginPermissionService {
    /**
     * 关联用户权限。
     *
     * @param form 用户权限表单
     */
    void addLoginPermission(LoginPermissionFormVo form);
}
