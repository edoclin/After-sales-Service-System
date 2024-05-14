package com.mi.aftersales.service.impl;

import com.mi.aftersales.entity.Login;
import com.mi.aftersales.mapper.LoginMapper;
import com.mi.aftersales.service.ILoginService;
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
public class LoginServiceImpl extends ServiceImpl<LoginMapper, Login> implements ILoginService {

}
