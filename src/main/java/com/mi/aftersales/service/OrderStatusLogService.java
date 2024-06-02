package com.mi.aftersales.service;

import com.mi.aftersales.pojo.vo.OrderStatusLogVo;

import java.util.List;

/**
 * <p>
 * 工单状态日志 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface OrderStatusLogService {
    /**
     * 查询工单状态日志。
     *
     * @param orderId 工单ID
     * @return 状态日志列表
     */
    List<OrderStatusLogVo> listOrderStatusLogByOrderId(String orderId);
}
