package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.Order;
import com.mi.aftersales.mapper.OrderMapper;
import com.mi.aftersales.repository.IOrderRepository;
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
public class IOrderRepositoryImpl extends ServiceImpl<OrderMapper, Order> implements IOrderRepository {

}
