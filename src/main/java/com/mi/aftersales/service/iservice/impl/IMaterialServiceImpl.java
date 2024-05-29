package com.mi.aftersales.service.iservice.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.mapper.MaterialMapper;
import com.mi.aftersales.service.iservice.IMaterialService;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 物料 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IMaterialServiceImpl extends ServiceImpl<MaterialMapper, Material> implements IMaterialService {
}
