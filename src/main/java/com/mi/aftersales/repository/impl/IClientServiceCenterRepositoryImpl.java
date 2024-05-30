package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.mapper.ClientServiceCenterMapper;
import com.mi.aftersales.repository.IClientServiceCenterRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 客户服务中心 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IClientServiceCenterRepositoryImpl extends ServiceImpl<ClientServiceCenterMapper, ClientServiceCenter> implements IClientServiceCenterRepository {
}
