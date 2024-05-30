package com.mi.aftersales.repository.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.mapper.MaterialMapper;
import com.mi.aftersales.repository.IMaterialRepository;
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
public class IMaterialRepositoryImpl extends ServiceImpl<MaterialMapper, Material> implements IMaterialRepository {
}
