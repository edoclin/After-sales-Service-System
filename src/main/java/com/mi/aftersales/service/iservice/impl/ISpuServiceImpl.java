package com.mi.aftersales.service.iservice.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.mapper.SpuMapper;
import com.mi.aftersales.service.iservice.ISpuService;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 商品 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ISpuServiceImpl extends ServiceImpl<SpuMapper, Spu> implements ISpuService {
}
