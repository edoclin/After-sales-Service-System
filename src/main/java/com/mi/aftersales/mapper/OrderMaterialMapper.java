package com.mi.aftersales.mapper;

import com.mi.aftersales.entity.OrderMaterial;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.mi.aftersales.pojo.vo.OrderMaterialVo;
import com.mi.aftersales.util.query.ConditionQuery;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * <p>
 * 工单物料中间表 Mapper 接口
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Mapper
public interface OrderMaterialMapper extends BaseMapper<OrderMaterial> {
    List<OrderMaterialVo> listApplyingOrderMaterial(ConditionQuery query);
    Long applyingOrderMaterialTotal(ConditionQuery query);
}
