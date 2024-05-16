package com.mi.aftersales.service.impl;

import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.mapper.SpuCategoryMapper;
import com.mi.aftersales.service.ISpuCategoryService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.vo.SpuCategory4ClientVo;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * <p>
 * 商品分类 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SpuCategoryServiceImpl extends ServiceImpl<SpuCategoryMapper, SpuCategory> implements ISpuCategoryService {

    @Resource
    private SpuCategoryMapper spuCategoryMapper;
    @Override
    public List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId) {
        return spuCategoryMapper.listSpuCategory4Client(parentId);
    }
}
