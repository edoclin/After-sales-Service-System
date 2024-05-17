package com.mi.aftersales.service;

import com.mi.aftersales.entity.SpuCategory;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.vo.result.SpuCategory4ClientVo;

import java.util.List;

/**
 * <p>
 * 商品分类 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface ISpuCategoryService extends IService<SpuCategory> {
    List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId);
}
