package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.Permission;
import com.mi.aftersales.mapper.PermissionMapper;
import com.mi.aftersales.service.iservice.IPermissionService;
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
public class IPermissionServiceImpl extends ServiceImpl<PermissionMapper, Permission> implements IPermissionService {

}
