package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.Sku;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SkuForm;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
import com.mi.aftersales.vo.result.ClientSkuVo;
import com.mi.aftersales.vo.result.SkuVo;

/**
 * <p>
 * 商品销售单元 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface ISkuService extends IService<Sku> {
    /**
     * 添加商品SKU。
     *
     * @param form SKU表单对象，包含SKU的详细信息。
     */
    void addSku(SkuForm form);

    /**
     * 更新商品SKU的可见性。
     *
     * @param form 更新SKU可见性表单对象，包含SKU ID和可见性状态。
     */
    void updateSkuVisibility(UpdateSkuVisibleForm form);

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
    void setSkuVisibility(SkuVisibleSetForm form);
}
