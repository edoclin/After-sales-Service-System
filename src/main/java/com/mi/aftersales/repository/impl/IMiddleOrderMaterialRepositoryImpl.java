package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.MiddleOrderMaterial;
import com.mi.aftersales.mapper.MiddleOrderMaterialMapper;
import com.mi.aftersales.repository.IMiddleOrderMaterialRepository;
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
public class IMiddleOrderMaterialRepositoryImpl extends ServiceImpl<MiddleOrderMaterialMapper, MiddleOrderMaterial> implements IMiddleOrderMaterialRepository {

}
