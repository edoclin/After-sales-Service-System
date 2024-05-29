package com.mi.aftersales.repository.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.mapper.LoginRoleMapper;
import com.mi.aftersales.repository.ILoginRoleRepository;
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
public class ILoginRoleRepositoryImpl extends ServiceImpl<LoginRoleMapper, LoginRole> implements ILoginRoleRepository {
}
