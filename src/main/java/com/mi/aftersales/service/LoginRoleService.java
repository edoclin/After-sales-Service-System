package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.LoginRoleForm;

/**
 * <p>
 * 员工信息表 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
public interface LoginRoleService{
    /**
     * 添加或更新用户角色。
     *
     * @param form 用户角色表单
     */
    void addOrUpdateLoginRole(LoginRoleForm form);
}
