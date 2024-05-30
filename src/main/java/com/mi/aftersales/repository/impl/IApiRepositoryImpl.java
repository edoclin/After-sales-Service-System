package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.Api;
import com.mi.aftersales.mapper.ApiMapper;
import com.mi.aftersales.repository.IApiRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * API列表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IApiRepositoryImpl extends ServiceImpl<ApiMapper, Api> implements IApiRepository {

}
