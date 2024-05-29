package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.Api;
import com.mi.aftersales.mapper.ApiMapper;
import com.mi.aftersales.service.iservice.IApiService;
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
public class ApiServiceImpl extends ServiceImpl<ApiMapper, Api> implements IApiService {

}
