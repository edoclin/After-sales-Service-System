package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SpuForm;
import com.mi.aftersales.vo.form.UpdateSpuVisibleForm;
import com.mi.aftersales.vo.result.ClientSpuVo;
import com.mi.aftersales.vo.result.SpuVo;

/**
 * <p>
 * 商品 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface SpuService {
    /**
     * 添加商品。
     *
     * @param form 商品表单对象
     */
    void addSpu(SpuForm form);

    /**
     * 更新商品可见性。
     *
     * @param form 更新商品可见性表单对象
     */
    void updateSpuVisibility(UpdateSpuVisibleForm form);

    /**
     * 分页查询客户可见的商品。
     *
     * @param query 查询条件对象
     * @param categoryId 商品所属分类ID
     * @return 客户可见的商品分页结果
     */
    PageResult<ClientSpuVo> listClientSpu(ConditionQuery query, Integer categoryId);

    /**
     * 分页查询管理员可见的商品。
     *
     * @param query 查询条件对象
     * @param categoryId 商品所属分类ID
     * @return 管理员可见的商品分页结果
     */
    PageResult<SpuVo> listSpu(ConditionQuery query, Integer categoryId);
}
