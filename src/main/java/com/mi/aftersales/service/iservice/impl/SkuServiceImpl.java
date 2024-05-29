package com.mi.aftersales.service.iservice.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mapper.SkuMapper;
import com.mi.aftersales.service.iservice.IFileService;
import com.mi.aftersales.service.iservice.ISkuService;
import com.mi.aftersales.service.iservice.ISpuService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.form.SkuForm;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.ClientSkuVo;
import com.mi.aftersales.vo.result.SkuVo;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
/**
 * <p>
 * 商品销售单元 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SkuServiceImpl extends ServiceImpl<SkuMapper, Sku> implements ISkuService {
    @Resource
    private SkuMapper skuMapper;

    @Resource
    private ISpuService iSpuService;

    @Resource
    private IFileService iFileService;

    @Override
    public void addSku(SkuForm form) {
        if (BeanUtil.isEmpty(iSpuService.getById(form.getSpuId()))) {
            throw new GracefulResponseException("商品SPU不存在！");
        }

        if (BeanUtil.isEmpty(iFileService.getById(form.getSkuCoverFileId()))) {
            throw new GracefulResponseException("商品SKU封面图片不存在！");
        }

        Sku sku = new Sku();
        BeanUtil.copyProperties(form, sku);
        try {
            skuMapper.insert(sku);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU名称重复！");
        }
    }

    @Override
    public void updateSkuVisibility(UpdateSkuVisibleForm form) {
        Sku sku = skuMapper.selectById(form.getSkuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        sku.setVisible(form.getVisible());
        if (skuMapper.updateById(sku) == 0) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuVo> listClientSku(ConditionQuery query, String spuId) {
        if (BeanUtil.isEmpty(iSpuService.getById(spuId))) {
            throw new GracefulResponseException("商品Spu不存在！");
        }

        PageResult<ClientSkuVo> result = new PageResult<>();
        result.setTotal(skuMapper.selectCount(new QueryWrapper<Sku>()
                .eq("spu_id", spuId)
                .eq("visible", true)));

        skuMapper.selectPage(new Page<>(query.getCurrent(), query.getLimit()), new QueryWrapper<Sku>()
                .eq("spu_id", spuId)
                .eq("visible", true)).getRecords().forEach(sku -> {
            ClientSkuVo clientSkuVo = new ClientSkuVo();
            BeanUtil.copyProperties(sku, clientSkuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileService.getById(sku.getSkuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                clientSkuVo.setSkuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(clientSkuVo);
        });

        return result;
    }

    @Override
    public PageResult<SkuVo> listSku(ConditionQuery query, String spuId) {
        if (BeanUtil.isEmpty(iSpuService.getById(spuId))) {
            throw new GracefulResponseException("商品所属Spu不存在！");
        }

        QueryWrapper<Sku> wrapper = QueryUtil.buildWrapper(query, Sku.class).eq("spu_id", spuId);

        PageResult<SkuVo> result = new PageResult<>();
        result.setTotal(skuMapper.selectCount(wrapper));

        skuMapper.selectPage(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(sku -> {
            SkuVo skuVo = new SkuVo();
            BeanUtil.copyProperties(sku, skuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileService.getById(sku.getSkuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                skuVo.setSkuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(skuVo);
        });

        return result;
    }

    @Override
    public void setSkuVisibility(SkuVisibleSetForm form) {
        Sku sku = skuMapper.selectById(form.getSkuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        sku.setVisible(form.getVisible());
        if (skuMapper.updateById(sku) == 0) {
            throw new ServerErrorException();
        }
    }
}
