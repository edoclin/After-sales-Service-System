package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SkuAttr;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mapper.SkuAttrMapper;
import com.mi.aftersales.mapper.SkuMapper;
import com.mi.aftersales.service.ISkuAttrService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SkuAttrForm;
import com.mi.aftersales.vo.form.SkuAttrVisibleSetForm;
import com.mi.aftersales.vo.result.ClientSkuAttrVo;
import com.mi.aftersales.vo.result.SkuAttrVo;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
public class SkuAttrServiceImpl extends ServiceImpl<SkuAttrMapper, SkuAttr> implements ISkuAttrService {
    @Resource
    private SkuAttrMapper skuAttrMapper;

    @Resource
    private SkuMapper skuMapper;

    @Override
    public void addSkuAttr(SkuAttrForm form) {
        if (BeanUtil.isEmpty(skuMapper.selectById(form.getSkuId()))) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        SkuAttr skuAttr = new SkuAttr();
        BeanUtil.copyProperties(form, skuAttr);
        try {
            skuAttrMapper.insert(skuAttr);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU属性名称重复！");
        }
    }

    @Override
    public void updateSkuAttrVisibility(SkuAttrVisibleSetForm form) {
        SkuAttr skuAttr = skuAttrMapper.selectById(form.getAttrId());
        if (BeanUtil.isEmpty(skuAttr)) {
            throw new GracefulResponseException("商品SKU属性不存在！");
        }

        skuAttr.setVisible(form.getVisible());
        if (skuAttrMapper.updateById(skuAttr) == 0) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuAttrVo> listClientSkuAttrs(ConditionQuery query, String skuId) {
        if (BeanUtil.isEmpty(skuMapper.selectById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        PageResult<ClientSkuAttrVo> result = new PageResult<>();
        result.setTotal(skuAttrMapper.selectCount(new QueryWrapper<SkuAttr>()
                .eq("sku_id", skuId)
                .eq("visible", true)));
        List<SkuAttr> skuAttrList = skuAttrMapper.selectPage(new Page<>(query.getCurrent(), query.getLimit()),
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
        if (BeanUtil.isEmpty(skuMapper.selectById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        QueryWrapper<SkuAttr> wrapper = QueryUtil.buildWrapper(query, SkuAttr.class).eq("sku_id", skuId);

        PageResult<SkuAttrVo> result = new PageResult<>();
        result.setTotal(skuAttrMapper.selectCount(wrapper));

        List<SkuAttr> skuAttrList = skuAttrMapper.selectPage(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords();

        skuAttrList.forEach(skuAttr -> {
            SkuAttrVo skuAttrVo = new SkuAttrVo();
            BeanUtil.copyProperties(skuAttr, skuAttrVo, DateUtil.copyDate2yyyyMMddHHmm());
            result.getData().add(skuAttrVo);
        });

        return result;
    }
}
