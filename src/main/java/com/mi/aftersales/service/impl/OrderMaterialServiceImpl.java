package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.entity.OrderMaterial;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import com.mi.aftersales.mapper.OrderMaterialMapper;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.OrderMaterialVo;
import com.mi.aftersales.pojo.vo.form.OrderMaterialGroupVo;
import com.mi.aftersales.repository.ILoginRepository;
import com.mi.aftersales.repository.IMaterialRepository;
import com.mi.aftersales.repository.IOrderMaterialRepository;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.service.OrderMaterialService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.util.view.ViewUtil;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <p>
 * 工单物料中间表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class OrderMaterialServiceImpl implements OrderMaterialService {

    @Resource
    private OrderMaterialMapper orderMaterialMapper;

    @Resource
    private IOrderMaterialRepository iOrderMaterialRepository;

    @Resource
    private ILoginRepository iLoginRepository;

    @Resource
    private IMaterialRepository iMaterialRepository;

    @Resource
    private IOrderRepository iOrderRepository;


    @Override
    public PageResult<OrderMaterialVo> listOrderMaterial(ConditionQuery query) {
        PageResult<OrderMaterialVo> result = new PageResult<>();
        result.setTotal(orderMaterialMapper.applyingOrderMaterialTotal(query));
        result.setDataColumns(ViewUtil.dataColumns(OrderMaterialVo.class));
        orderMaterialMapper.listApplyingOrderMaterial(query).forEach(orderMaterial -> {
            OrderMaterialVo item = new OrderMaterialVo();
            BeanUtil.copyProperties(orderMaterial, item, DateUtil.copyDate2yyyyMMddHHmm());
            Material material = iMaterialRepository.getById(orderMaterial.getMaterialId());
            if (BeanUtil.isNotEmpty(material)) {
                item.setMaterialName(material.getMaterialName());
            }
            Login login = iLoginRepository.getById(orderMaterial.getCreatedId());
            if (BeanUtil.isNotEmpty(login)) {
                item.setMobile(login.getMobile());
            }
            result.getData().add(item);
        });
        return result;
    }


    @Override
    public PageResult<OrderMaterialGroupVo> listOrderMaterialGroupByOrder(ConditionQuery query) {
        PageResult<OrderMaterialGroupVo> result = new PageResult<>();

        QueryWrapper<Order> wrapper = QueryUtil.buildWrapper(query, Order.class);

        wrapper = wrapper.eq("order_status", OrderStatusEnum.MATERIAL_APPLYING);
        result.setTotal(iOrderRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(OrderMaterialGroupVo.class));

        iOrderRepository
                .page(new Page<>(query.getCurrent(), query.getLimit()), wrapper)
                .getRecords().forEach(order -> {
                    OrderMaterialGroupVo item = new OrderMaterialGroupVo();
                    BeanUtil.copyProperties(order, item, DateUtil.copyDate2yyyyMMddHHmm());
                    item.getOrderMaterialVoList().setDataColumns(ViewUtil.dataColumns(OrderMaterialVo.class));

                    iOrderMaterialRepository
                            .lambdaQuery()
                            .eq(OrderMaterial::getOrderId, order.getOrderId())
                            .list().forEach(orderMaterial -> {

                                OrderMaterialVo vo = new OrderMaterialVo();

                                BeanUtil.copyProperties(orderMaterial, vo, DateUtil.copyDate2yyyyMMddHHmm());
                                Material material = iMaterialRepository.getById(orderMaterial.getMaterialId());
                                if (BeanUtil.isNotEmpty(material)) {
                                    vo.setMaterialName(material.getMaterialName());
                                }
                                Login login = iLoginRepository.getById(orderMaterial.getCreatedId());
                                if (BeanUtil.isNotEmpty(login)) {
                                    vo.setMobile(login.getMobile());
                                }
                                item.getOrderMaterialVoList().getData().add(vo);
                            });
                    result.getData().add(item);
                });
        return result;
    }
}
