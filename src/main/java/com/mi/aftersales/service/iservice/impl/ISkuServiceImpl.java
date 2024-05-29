package com.mi.aftersales.service.iservice.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.mapper.SkuMapper;
import com.mi.aftersales.service.iservice.ISkuService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
/**
 * <p>
 * 商品销售单元 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ISkuServiceImpl extends ServiceImpl<SkuMapper, Sku> implements ISkuService {
}
