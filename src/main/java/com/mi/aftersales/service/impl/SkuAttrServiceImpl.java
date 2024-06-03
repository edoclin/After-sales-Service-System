package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SkuAttr;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.SkuAttrService;
import com.mi.aftersales.repository.ISkuAttrRepository;
import com.mi.aftersales.repository.ISkuRepository;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuAttrFormVo;
import com.mi.aftersales.pojo.vo.form.SkuAttrVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuAttrVo;
import com.mi.aftersales.pojo.vo.SkuAttrVo;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
/**
 * <p>
 * sku属性 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SkuAttrServiceImpl implements SkuAttrService {
    @Resource
    private ISkuAttrRepository iSkuAttrRepository;

    @Resource
    private ISkuRepository iSkuRepository;

    @Override
    public void addSkuAttr(SkuAttrFormVo form) {
        if (BeanUtil.isEmpty(iSkuRepository.getById(form.getSkuId()))) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        SkuAttr skuAttr = new SkuAttr();
        BeanUtil.copyProperties(form, skuAttr);
        try {
            iSkuAttrRepository.save(skuAttr);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU属性名称重复！");
        }
    }

    @Override
    public void updateSkuAttrVisibility(SkuAttrVisibleSetFormVo form) {
        SkuAttr skuAttr = iSkuAttrRepository.getById(form.getAttrId());
        if (BeanUtil.isEmpty(skuAttr)) {
            throw new GracefulResponseException("商品SKU属性不存在！");
        }

        skuAttr.setVisible(form.getVisible());
        if (!iSkuAttrRepository.updateById(skuAttr)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuAttrVo> listClientSkuAttrs(ConditionQuery query, String skuId) {
        if (BeanUtil.isEmpty(iSkuRepository.getById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        PageResult<ClientSkuAttrVo> result = new PageResult<>();
        result.setTotal(iSkuAttrRepository.count(new QueryWrapper<SkuAttr>()
                .eq("sku_id", skuId)
                .eq("visible", true)));
        List<SkuAttr> skuAttrList = iSkuAttrRepository.page(new Page<>(query.getCurrent(), query.getLimit()),
                new QueryWrapper<SkuAttr>()
                        .eq("sku_id", skuId)
                        .eq("visible", true)).getRecords();

        skuAttrList.forEach(skuAttr -> {
            ClientSkuAttrVo clientSkuAttrVo = new ClientSkuAttrVo();
            BeanUtil.copyProperties(skuAttr, clientSkuAttrVo);
            result.getData().add(clientSkuAttrVo);
        });

        return result;
    }

    @Override
    public PageResult<SkuAttrVo> listSkuAttrs(ConditionQuery query, String skuId) {
        if (BeanUtil.isEmpty(iSkuRepository.getById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        QueryWrapper<SkuAttr> wrapper = QueryUtil.buildWrapper(query, SkuAttr.class).eq("sku_id", skuId);

        PageResult<SkuAttrVo> result = new PageResult<>();
        result.setTotal(iSkuAttrRepository.count(wrapper));

        List<SkuAttr> skuAttrList = iSkuAttrRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords();

        skuAttrList.forEach(skuAttr -> {
            SkuAttrVo skuAttrVo = new SkuAttrVo();
            BeanUtil.copyProperties(skuAttr, skuAttrVo, DateUtil.copyDate2yyyyMMddHHmm());
            result.getData().add(skuAttrVo);
        });

        return result;
    }
}
