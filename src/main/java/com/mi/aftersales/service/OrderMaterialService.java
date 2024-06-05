package com.mi.aftersales.service;

import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.OrderMaterialVo;
import com.mi.aftersales.pojo.vo.form.OrderMaterialGroupVo;
import com.mi.aftersales.util.query.ConditionQuery;

/**
 * <p>
 * 工单物料中间表 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface OrderMaterialService {

    PageResult<OrderMaterialVo> listOrderMaterial(ConditionQuery query);

    PageResult<OrderMaterialGroupVo> listOrderMaterialGroupByOrder(ConditionQuery query);
}
