package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.SkuService;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.ISkuRepository;
import com.mi.aftersales.repository.ISpuRepository;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.SkuVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuVo;
import com.mi.aftersales.pojo.vo.SkuVo;
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
public class SkuServiceImpl implements SkuService {
    @Resource
    private ISkuRepository iSkuRepository;

    @Resource
    private ISpuRepository iSpuRepository;

    @Resource
    private IFileRepository iFileRepository;

    @Override
    public void addSku(SkuFormVo form) {
        if (BeanUtil.isEmpty(iSpuRepository.getById(form.getSpuId()))) {
            throw new GracefulResponseException("商品SPU不存在！");
        }

        if (BeanUtil.isEmpty(iFileRepository.getById(form.getSkuCoverFileId()))) {
            throw new GracefulResponseException("商品SKU封面图片不存在！");
        }

        Sku sku = new Sku();
        BeanUtil.copyProperties(form, sku);
        try {
            iSkuRepository.save(sku);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU名称重复！");
        }
    }

    @Override
    public void updateSkuVisibility(UpdateSkuVisibleFormVo form) {
        Sku sku = iSkuRepository.getById(form.getSkuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        sku.setVisible(form.getVisible());
        if (!iSkuRepository.updateById(sku)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuVo> listClientSku(ConditionQuery query, String spuId) {
        if (BeanUtil.isEmpty(iSpuRepository.getById(spuId))) {
            throw new GracefulResponseException("商品Spu不存在！");
        }

        PageResult<ClientSkuVo> result = new PageResult<>();
        result.setTotal(iSkuRepository.count(new QueryWrapper<Sku>()
                .eq("spu_id", spuId)
                .eq("visible", true)));

        iSkuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), new QueryWrapper<Sku>()
                .eq("spu_id", spuId)
                .eq("visible", true)).getRecords().forEach(sku -> {
            ClientSkuVo clientSkuVo = new ClientSkuVo();
            BeanUtil.copyProperties(sku, clientSkuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileRepository.getById(sku.getSkuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                clientSkuVo.setSkuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(clientSkuVo);
        });

        return result;
    }

    @Override
    public PageResult<SkuVo> listSku(ConditionQuery query, String spuId) {
        if (BeanUtil.isEmpty(iSpuRepository.getById(spuId))) {
            throw new GracefulResponseException("商品所属Spu不存在！");
        }

        QueryWrapper<Sku> wrapper = QueryUtil.buildWrapper(query, Sku.class).eq("spu_id", spuId);

        PageResult<SkuVo> result = new PageResult<>();
        result.setTotal(iSkuRepository.count(wrapper));

        iSkuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(sku -> {
            SkuVo skuVo = new SkuVo();
            BeanUtil.copyProperties(sku, skuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileRepository.getById(sku.getSkuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                skuVo.setSkuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(skuVo);
        });

        return result;
    }

    @Override
    public void setSkuVisibility(SkuVisibleSetFormVo form) {
        Sku sku = iSkuRepository.getById(form.getSkuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        sku.setVisible(form.getVisible());
        if (!iSkuRepository.updateById(sku)) {
            throw new ServerErrorException();
        }
    }
}
