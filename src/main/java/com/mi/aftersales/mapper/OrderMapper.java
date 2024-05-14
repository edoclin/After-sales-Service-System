package com.mi.aftersales.mapper;

import com.mi.aftersales.entity.Order;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * <p>
 * 工单 Mapper 接口
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Mapper
public interface OrderMapper extends BaseMapper<Order> {

}
