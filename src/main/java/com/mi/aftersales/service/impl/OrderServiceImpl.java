package com.mi.aftersales.service.impl;

import com.mi.aftersales.entity.Order;
import com.mi.aftersales.mapper.OrderMapper;
import com.mi.aftersales.service.IOrderService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements IOrderService {

}
