package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.mapper.PayOrderMapper;
import com.mi.aftersales.service.iservice.IPayOrderService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单支付记录 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class PayOrderServiceImpl extends ServiceImpl<PayOrderMapper, PayOrder> implements IPayOrderService {

}
