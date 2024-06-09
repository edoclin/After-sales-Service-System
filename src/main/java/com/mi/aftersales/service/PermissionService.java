package com.mi.aftersales.service;

import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.exception.graceful.TransactionalErrorException;
import com.mi.aftersales.pojo.vo.PermissionVo;
import com.mi.aftersales.pojo.vo.form.PermissionApiFormVo;
import com.mi.aftersales.pojo.vo.form.UpdatePermissionApiFormVo;
import com.mi.aftersales.util.query.ConditionQuery;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 权限 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface PermissionService {

    PageResult<PermissionVo> listPermission(ConditionQuery query);

    @Transactional(rollbackFor = TransactionalErrorException.class)
    void addPermission(PermissionApiFormVo formVo);

    void removePermissionById(String permissionId);

    @Transactional(rollbackFor = TransactionalErrorException.class)
    void updatePermissionById(UpdatePermissionApiFormVo form);
}
