package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.OrderMaterial;
import com.mi.aftersales.mapper.OrderMaterialMapper;
import com.mi.aftersales.repository.IOrderMaterialRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单物料中间表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IOrderMaterialRepositoryImpl extends ServiceImpl<OrderMaterialMapper, OrderMaterial> implements IOrderMaterialRepository {

}
