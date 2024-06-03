package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SpuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSpuVo;
import com.mi.aftersales.pojo.vo.SpuVo;

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
    void addSpu(SpuFormVo form);

    /**
     * 更新商品可见性。
     *
     * @param form 更新商品可见性表单对象
     */
    void updateSpuVisibility(UpdateSpuVisibleFormVo form);

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
