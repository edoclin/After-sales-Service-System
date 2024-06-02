package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.SkuVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuVo;
import com.mi.aftersales.pojo.vo.SkuVo;

/**
 * <p>
 * 商品销售单元 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface SkuService {
    /**
     * 添加商品SKU。
     *
     * @param form SKU表单对象，包含SKU的详细信息。
     */
    void addSku(SkuFormVo form);

    /**
     * 更新商品SKU的可见性。
     *
     * @param form 更新SKU可见性表单对象，包含SKU ID和可见性状态。
     */
    void updateSkuVisibility(UpdateSkuVisibleFormVo form);

    /**
     * 分页查询客户可见的商品SKU。
     *
     * @param query 查询条件对象，包含分页信息。
     * @param spuId 商品所属的SPU ID。
     * @return 分页结果，包含客户可见的SKU列表。
     */
    PageResult<ClientSkuVo> listClientSku(ConditionQuery query, String spuId);

    /**
     * 分页查询所有商品SKU（管理员）。
     *
     * @param query 查询条件对象，包含分页信息。
     * @param spuId 商品所属的SPU ID。
     * @return 分页结果，包含SKU列表。
     */
    PageResult<SkuVo> listSku(ConditionQuery query, String spuId);

    /**
     * 设置商品SKU的可见性（管理员）。
     *
     * @param form SKU可见性设置表单对象，包含SKU ID和可见性状态。
     */
    void setSkuVisibility(SkuVisibleSetFormVo form);
}
