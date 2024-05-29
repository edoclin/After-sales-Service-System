package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.MiddleLoginPermission;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.vo.form.LoginPermissionForm;

/**
 * <p>
 * 用户具有权限中间表 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface IMiddleLoginPermissionService extends IService<MiddleLoginPermission> {
    /**
     * 关联用户权限。
     *
     * @param form 用户权限表单
     */
    void addLoginPermission(LoginPermissionForm form);
}
