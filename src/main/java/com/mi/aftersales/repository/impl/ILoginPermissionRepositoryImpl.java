package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.LoginPermission;
import com.mi.aftersales.mapper.MiddleLoginPermissionMapper;
import com.mi.aftersales.repository.ILoginPermissionRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 用户具有权限中间表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ILoginPermissionRepositoryImpl extends ServiceImpl<MiddleLoginPermissionMapper, LoginPermission> implements ILoginPermissionRepository {
}
