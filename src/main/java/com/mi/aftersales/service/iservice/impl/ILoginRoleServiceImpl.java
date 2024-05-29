package com.mi.aftersales.service.iservice.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.mapper.LoginRoleMapper;
import com.mi.aftersales.service.iservice.ILoginRoleService;
import org.springframework.stereotype.Service;
/**
 * <p>
 * 员工信息表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
@Service
public class ILoginRoleServiceImpl extends ServiceImpl<LoginRoleMapper, LoginRole> implements ILoginRoleService {
}
