package com.mi.aftersales.repository;

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
public interface ISpuCategoryRepository extends IService<SpuCategory> {
    /**
     * 获取客户可见的SPU分类目录。
     *
     * @param parentId 父级分类ID
     * @return 客户可见的SPU分类目录列表
     */
    List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId);

    List<String> listAllSpuCategoryName(Integer categoryId);
}
