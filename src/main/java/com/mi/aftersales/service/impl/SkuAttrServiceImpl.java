package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.mi.aftersales.entity.SkuAttr;
import com.mi.aftersales.exception.graceful.IllegalSkuIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.exception.graceful.alias.IllegalSkuAttrIdException;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.ClientSkuAttrVo;
import com.mi.aftersales.pojo.vo.SkuAttrVo;
import com.mi.aftersales.pojo.vo.form.SkuAttrFormVo;
import com.mi.aftersales.pojo.vo.form.SkuAttrVisibleSetFormVo;
import com.mi.aftersales.repository.ISkuAttrRepository;
import com.mi.aftersales.repository.ISkuRepository;
import com.mi.aftersales.service.SkuAttrService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.util.view.ViewUtil;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

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
            throw new IllegalSkuIdException();
        }

        SkuAttr skuAttr = new SkuAttr();
        BeanUtil.copyProperties(form, skuAttr);

        iSkuAttrRepository.save(skuAttr);
    }

    @Override
    public void updateSkuAttrVisibility(SkuAttrVisibleSetFormVo form) {
        SkuAttr skuAttr = iSkuAttrRepository.getById(form.getAttrId());
        if (BeanUtil.isEmpty(skuAttr)) {
            throw new IllegalSkuAttrIdException();
        }

        skuAttr.setVisible(form.getVisible());
        if (!iSkuAttrRepository.updateById(skuAttr)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuAttrVo> listClientSkuAttrs(ConditionQuery query, String skuId) {
        if (BeanUtil.isEmpty(iSkuRepository.getById(skuId))) {
            throw new IllegalSkuIdException();
        }

        PageResult<ClientSkuAttrVo> result = new PageResult<>();


        QueryWrapper<SkuAttr> wrapper = QueryUtil.buildEmptyQueryWrapper(SkuAttr.class);
        wrapper = wrapper.eq("sku_id", skuId)
                .eq("visible", true);
        result.setTotal(iSkuAttrRepository.count(wrapper));
         iSkuAttrRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords()
        .forEach(skuAttr -> {
            ClientSkuAttrVo clientSkuAttrVo = new ClientSkuAttrVo();
            BeanUtil.copyProperties(skuAttr, clientSkuAttrVo);
            result.getData().add(clientSkuAttrVo);
        });

        return result;
    }

    @Override
    public PageResult<SkuAttrVo> listSkuAttrs(ConditionQuery query) {


        QueryWrapper<SkuAttr> wrapper = QueryUtil.buildWrapper(query, SkuAttr.class);

        PageResult<SkuAttrVo> result = new PageResult<>();
        result.setTotal(iSkuAttrRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(SkuAttr.class));

        iSkuAttrRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords()
        .forEach(skuAttr -> {
            SkuAttrVo skuAttrVo = new SkuAttrVo();
            BeanUtil.copyProperties(skuAttr, skuAttrVo, DateUtil.copyDate2yyyyMMddHHmm());
            result.getData().add(skuAttrVo);
        });

        return result;
    }
}
