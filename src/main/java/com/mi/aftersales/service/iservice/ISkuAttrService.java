package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.SkuAttr;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SkuAttrForm;
import com.mi.aftersales.vo.form.SkuAttrVisibleSetForm;
import com.mi.aftersales.vo.result.ClientSkuAttrVo;
import com.mi.aftersales.vo.result.SkuAttrVo;

/**
 * <p>
 * sku属性 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface ISkuAttrService extends IService<SkuAttr> {
    /**
     * 添加SKU属性。
     *
     * @param form SKU属性表单
     */
    void addSkuAttr(SkuAttrForm form);

    /**
     * 更新SKU属性可见性。
     *
     * @param form SKU属性可见性表单
     */
    void updateSkuAttrVisibility(SkuAttrVisibleSetForm form);

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
     * @param skuId SKU ID
     * @return SKU属性分页结果
     */
    PageResult<SkuAttrVo> listSkuAttrs(ConditionQuery query, String skuId);
}
