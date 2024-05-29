package com.mi.aftersales.service.iservice.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.mapper.OrderStatusLogMapper;
import com.mi.aftersales.service.iservice.IOrderStatusLogService;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单状态日志 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IOrderStatusLogServiceImpl extends ServiceImpl<OrderStatusLogMapper, OrderStatusLog> implements IOrderStatusLogService {
}
