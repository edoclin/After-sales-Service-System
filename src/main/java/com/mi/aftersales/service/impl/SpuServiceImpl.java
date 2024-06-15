package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.exception.graceful.IllegalSpuIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.pojo.vo.form.UpdateSpuFormVo;
import com.mi.aftersales.repository.ISkuRepository;
import com.mi.aftersales.service.SpuService;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.repository.ISpuRepository;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SpuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSpuVo;
import com.mi.aftersales.pojo.vo.SpuVo;
import com.mi.aftersales.util.view.ViewUtil;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <p>
 * 商品 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SpuServiceImpl implements SpuService {
    @Resource
    private ISpuRepository iSpuRepository;

    @Resource
    private ISpuCategoryRepository iSpuCategoryRepository;

    @Resource
    private ISkuRepository iSkuRepository;

    @Resource
    private IFileRepository iFileRepository;

    @Override
    public void addSpu(SpuFormVo form) {
        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getCategoryId()))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }

        if (BeanUtil.isEmpty(iFileRepository.getById(form.getSpuCoverFileId()))) {
            throw new GracefulResponseException("商品封面图片不存在！");
        }

        Spu spu = new Spu();
        BeanUtil.copyProperties(form, spu);

        try {
            iSpuRepository.save(spu);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品名称重复！");
        }
    }

    @Override
    public void updateSpuVisibility(UpdateSpuVisibleFormVo form) {
        Spu spu = iSpuRepository.getById(form.getSpuId());
        if (BeanUtil.isEmpty(spu)) {
            throw new IllegalSpuIdException();
        }

        spu.setVisible(form.getVisible());
        if (!iSpuRepository.updateById(spu)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public void removeSpuById(String spuId) {

        Spu spu = iSpuRepository.getById(spuId);
        if (BeanUtil.isEmpty(spu)) {
            throw new IllegalSpuIdException();
        }

        if (CollUtil.isNotEmpty(iSkuRepository.lambdaQuery().eq(Sku::getSpuId, spuId).list())) {
            throw new GracefulResponseException("存在相关联的商品Sku，无法删除！");
        }

        iSpuRepository.removeById(spuId);
    }

    @Override
    public void updateSpuById(UpdateSpuFormVo form) {
        Spu spu = iSpuRepository.getById(form.getSpuId());
        if (BeanUtil.isEmpty(spu)) {
            throw new IllegalSpuIdException();
        }

        spu.setVisible(form.getVisible());
        if (!iSpuRepository.updateById(spu)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSpuVo> listClientSpu(ConditionQuery query, Integer categoryId) {
        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(categoryId))) {
            throw new IllegalSpuIdException();
        }

        QueryWrapper<Spu> wrapper = QueryUtil.buildWrapper(query, Spu.class);
        wrapper = wrapper.eq("category_id", categoryId)
                .eq("visible", true);
        PageResult<ClientSpuVo> result = new PageResult<>();
        result.setTotal(iSpuRepository.count(wrapper));

        iSpuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(spu -> {
            ClientSpuVo clientSpuVo = new ClientSpuVo();
            BeanUtil.copyProperties(spu, clientSpuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileRepository.getById(spu.getSpuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                clientSpuVo.setSpuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(clientSpuVo);
        });

        return result;
    }

    @Override
    public PageResult<SpuVo> listSpu(ConditionQuery query) {
        QueryWrapper<Spu> wrapper = QueryUtil.buildWrapper(query, Spu.class);
        PageResult<SpuVo> result = new PageResult<>();
        result.setTotal(iSpuRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(SpuVo.class));

        iSpuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(spu -> {
            SpuVo spuVo = new SpuVo();
            BeanUtil.copyProperties(spu, spuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileRepository.getById(spu.getSpuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                spuVo.setSpuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(spuVo);
        });

        return result;
    }
}
