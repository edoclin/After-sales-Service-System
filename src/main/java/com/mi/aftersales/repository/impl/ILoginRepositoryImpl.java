package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.Login;
import com.mi.aftersales.mapper.LoginMapper;
import com.mi.aftersales.repository.ILoginRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 登录表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ILoginRepositoryImpl extends ServiceImpl<LoginMapper, Login> implements ILoginRepository {

}
