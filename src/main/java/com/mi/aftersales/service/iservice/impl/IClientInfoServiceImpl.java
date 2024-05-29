package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.ClientInfo;
import com.mi.aftersales.mapper.ClientInfoMapper;
import com.mi.aftersales.service.iservice.IClientInfoService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 客户信息表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IClientInfoServiceImpl extends ServiceImpl<ClientInfoMapper, ClientInfo> implements IClientInfoService {

}
