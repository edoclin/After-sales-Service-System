package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.MiddleLoginPermission;
import com.mi.aftersales.mapper.MiddleLoginPermissionMapper;
import com.mi.aftersales.service.iservice.IMiddleLoginPermissionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
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
public class IMiddleLoginPermissionServiceImpl extends ServiceImpl<MiddleLoginPermissionMapper, MiddleLoginPermission> implements IMiddleLoginPermissionService {
}
