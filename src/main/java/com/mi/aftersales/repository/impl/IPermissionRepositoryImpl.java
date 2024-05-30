package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.mapper.PermissionMapper;
import com.mi.aftersales.repository.IPermissionRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 权限 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IPermissionRepositoryImpl extends ServiceImpl<PermissionMapper, Permission> implements IPermissionRepository {

}
