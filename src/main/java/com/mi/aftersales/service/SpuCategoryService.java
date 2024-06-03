package com.mi.aftersales.service;

import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.SpuCategory4ClientVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuCategoryFormVo;

import java.util.List;

/**
 * <p>
 * 商品分类 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface SpuCategoryService {
    /**
     * 添加SPU分类。
     *
     * @param form SPU分类表单对象
     */
    void addSpuCategory(SpuCategoryFormVo form);

    void updateSpuCategory(UpdateSpuCategoryFormVo form);

    void deleteSpuCategoryById(Integer categoryId);

    List<Integer> childrenCategoryId(Integer parentCategoryId);

    /**
     * 设置SPU分类是否对客户可见。
     *
     * @param form SPU分类可见性设置表单对象
     */
    void setSpuCategoryVisibility(SpuCategoryVisibleSetFormVo form);
    /**
     * 获取客户可见的SPU分类目录。
     *
     * @param parentId 父级分类ID
     * @return 客户可见的SPU分类目录列表
     */
    List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId);
}
