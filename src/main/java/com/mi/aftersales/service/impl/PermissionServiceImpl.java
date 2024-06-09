package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.entity.PermissionApi;
import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.exception.graceful.IllegalApiIdException;
import com.mi.aftersales.exception.graceful.IllegalPermissionIdException;
import com.mi.aftersales.exception.graceful.TransactionalErrorException;
import com.mi.aftersales.pojo.vo.PermissionVo;
import com.mi.aftersales.pojo.vo.form.PermissionApiFormVo;
import com.mi.aftersales.pojo.vo.form.UpdatePermissionApiFormVo;
import com.mi.aftersales.repository.IApiRepository;
import com.mi.aftersales.repository.IPermissionApiRepository;
import com.mi.aftersales.repository.IPermissionRepository;
import com.mi.aftersales.service.PermissionService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.util.view.ViewUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 权限 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class PermissionServiceImpl implements PermissionService {

    @Resource
    private IPermissionRepository iPermissionRepository;

    @Resource
    private IPermissionApiRepository iPermissionApiRepository;

    @Resource
    private IApiRepository iApiRepository;

    @Override
    public PageResult<PermissionVo> listPermission(ConditionQuery query) {
        PageResult<PermissionVo> result = new PageResult<>();

        QueryWrapper<Permission> wrapper = QueryUtil.buildWrapper(query, Permission.class);

        result.setTotal(iPermissionRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(PermissionVo.class));

        iPermissionRepository.page(QueryUtil.page(query), wrapper).getRecords().forEach(permission -> {
            PermissionVo permissionVo = new PermissionVo();
            BeanUtil.copyProperties(permission, permissionVo, DateUtil.copyDate2yyyyMMddHHmm());
            result.getData().add(permissionVo);
        });

        return result;
    }

    @Override
    @Transactional(rollbackFor = TransactionalErrorException.class)
    public void addPermission(PermissionApiFormVo form) {
        Permission permission = new Permission();
        BeanUtil.copyProperties(form, permission);

        if (iPermissionRepository.save(permission)) {
            setRelatedApis(permission, form.getApiIds());
        }
    }


    @Override
    public void removePermissionById(String permissionId) {
        if (BeanUtil.isEmpty(iPermissionRepository.getById(permissionId))) {
            throw new IllegalPermissionIdException();
        }
        iPermissionRepository.removeById(permissionId);
    }

    @Override
    @Transactional(rollbackFor = TransactionalErrorException.class)
    public void updatePermissionById(UpdatePermissionApiFormVo form) {
        Permission permission = iPermissionRepository.getById(form.getPermissionId());
        if (BeanUtil.isEmpty(permission)) {
            throw new IllegalPermissionIdException();
        }

        iPermissionApiRepository.lambdaUpdate()
                .eq(PermissionApi::getPermissionId, form.getPermissionId())
                .remove();

        if (iPermissionRepository.updateById(permission)) {
            setRelatedApis(permission, form.getApiIds());
        }
    }

    private void setRelatedApis(Permission permission, List<Integer> apiIds) {
        List<PermissionApi> batch = new ArrayList<>();
        apiIds.forEach(apiId -> {
            if (BeanUtil.isEmpty(iApiRepository.getById(apiId))) {
                throw new IllegalApiIdException();
            }

            PermissionApi item = new PermissionApi();
            item.setPermissionId(permission.getPermissionId())
                    .setApiId(apiId);

            batch.add(item);
        });
        iPermissionApiRepository.saveBatch(batch);
    }
}
