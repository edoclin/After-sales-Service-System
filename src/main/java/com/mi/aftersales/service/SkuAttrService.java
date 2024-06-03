package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuAttrFormVo;
import com.mi.aftersales.pojo.vo.form.SkuAttrVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuAttrVo;
import com.mi.aftersales.pojo.vo.SkuAttrVo;

/**
 * <p>
 * sku属性 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface SkuAttrService {
    /**
     * 添加SKU属性。
     *
     * @param form SKU属性表单
     */
    void addSkuAttr(SkuAttrFormVo form);

    /**
     * 更新SKU属性可见性。
     *
     * @param form SKU属性可见性表单
     */
    void updateSkuAttrVisibility(SkuAttrVisibleSetFormVo form);

    /**
     * 查询客户SKU属性。
     *
     * @param query 查询条件
     * @param skuId SKU ID
     * @return SKU属性分页结果
     */
    PageResult<ClientSkuAttrVo> listClientSkuAttrs(ConditionQuery query, String skuId);

    /**
     * 查询管理员SKU属性。
     *
     * @param query 查询条件
     * @return SKU属性分页结果
     */
    PageResult<SkuAttrVo> listSkuAttrs(ConditionQuery query);
}
