package com.mi.aftersales.repository.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.mapper.SpuMapper;
import com.mi.aftersales.repository.ISpuRepository;
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
public class ISpuRepositoryImpl extends ServiceImpl<SpuMapper, Spu> implements ISpuRepository {
}
