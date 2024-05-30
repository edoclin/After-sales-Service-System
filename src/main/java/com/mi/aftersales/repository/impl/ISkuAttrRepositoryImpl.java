package com.mi.aftersales.repository.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.SkuAttr;
import com.mi.aftersales.mapper.SkuAttrMapper;
import com.mi.aftersales.repository.ISkuAttrRepository;
import org.springframework.stereotype.Service;

/**
 * <p>
 * sku属性 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ISkuAttrRepositoryImpl extends ServiceImpl<SkuAttrMapper, SkuAttr> implements ISkuAttrRepository {
}
