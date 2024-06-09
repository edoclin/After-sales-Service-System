package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.PermissionApi;
import com.mi.aftersales.mapper.MiddlePermissionApiMapper;
import com.mi.aftersales.repository.IPermissionApiRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 权限具有API中间表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IPermissionApiRepositoryImpl extends ServiceImpl<MiddlePermissionApiMapper, PermissionApi> implements IPermissionApiRepository {

}
