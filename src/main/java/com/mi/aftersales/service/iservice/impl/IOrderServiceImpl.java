package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.Order;
import com.mi.aftersales.mapper.OrderMapper;
import com.mi.aftersales.service.iservice.*;
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
public class IOrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements IOrderService {

}
