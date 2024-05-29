package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.SpuCategory;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.vo.form.SpuCategoryForm;
import com.mi.aftersales.vo.form.SpuCategoryVisibleSetForm;
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
    /**
     * 添加SPU分类。
     *
     * @param form SPU分类表单对象
     */
    void addSpuCategory(SpuCategoryForm form);

    /**
     * 设置SPU分类是否对客户可见。
     *
     * @param form SPU分类可见性设置表单对象
     */
    void setSpuCategoryVisibility(SpuCategoryVisibleSetForm form);
    /**
     * 获取客户可见的SPU分类目录。
     *
     * @param parentId 父级分类ID
     * @return 客户可见的SPU分类目录列表
     */
    List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId);

    List<String> listAllSpuCategoryName(Integer categoryId);
}
