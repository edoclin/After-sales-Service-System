package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.exception.graceful.*;
import com.mi.aftersales.pojo.vo.form.UpdateSkuFormVo;
import com.mi.aftersales.service.SkuService;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.ISkuRepository;
import com.mi.aftersales.repository.ISpuRepository;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuVo;
import com.mi.aftersales.pojo.vo.SkuVo;
import com.mi.aftersales.util.view.ViewUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
    private static final Logger log = LoggerFactory.getLogger(SkuServiceImpl.class);
    @Resource
    private ISkuRepository iSkuRepository;

    @Resource
    private ISpuRepository iSpuRepository;

    @Resource
    private IFileRepository iFileRepository;

    @Override
    public void addSku(SkuFormVo form) {
        if (BeanUtil.isEmpty(iSpuRepository.getById(form.getSpuId()))) {
            throw new IllegalSpuIdException();
        }

        if (BeanUtil.isEmpty(iFileRepository.getById(form.getSkuCoverFileId()))) {
            throw new IllegalFileIdException();
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
    public void updateSkuById(UpdateSkuFormVo form) {
        Sku sku = iSkuRepository.getById(form.getSpuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new IllegalSkuIdException();
        }

        if (BeanUtil.isEmpty(iFileRepository.getById(form.getSkuCoverFileId()))) {
            throw new IllegalFileIdException();
        }
        BeanUtil.copyProperties(form, sku, CopyOptions.create().ignoreNullValue());

        try {
            iSkuRepository.updateById(sku);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public void updateSkuVisibility(UpdateSkuVisibleFormVo form) {
        Sku sku = iSkuRepository.getById(form.getSkuId());
        if (BeanUtil.isEmpty(sku)) {
            throw new IllegalSkuIdException();
        }

        sku.setVisible(form.getVisible());

        try {
            iSkuRepository.updateById(sku);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSkuVo> listClientSku(ConditionQuery query, String spuId) {
        PageResult<ClientSkuVo> result = new PageResult<>();

        QueryWrapper<Sku> wrapper = QueryUtil.buildEmptyQueryWrapper(Sku.class);

        wrapper = wrapper.eq("spu_id", spuId).eq("visible", true);

        result.setTotal(iSkuRepository.count(wrapper));

        iSkuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords()

                .forEach(sku -> {
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
    public PageResult<SkuVo> conditionList(ConditionQuery query) {
        QueryWrapper<Sku> wrapper = QueryUtil.buildWrapper(query, Sku.class);

        PageResult<SkuVo> result = new PageResult<>();
        result.setTotal(iSkuRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(SkuVo.class));

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
    public void removeSkuById(String skuId) {
        Sku sku = iSkuRepository.getById(skuId);

        if (BeanUtil.isEmpty(sku)) {
            throw new IllegalSkuIdException();
        }

        try {
            iSkuRepository.removeById(skuId);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }
}
