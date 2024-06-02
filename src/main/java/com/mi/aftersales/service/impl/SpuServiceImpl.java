package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.SpuService;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.repository.ISpuRepository;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SpuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuVisibleFormVo;
import com.mi.aftersales.pojo.vo.ClientSpuVo;
import com.mi.aftersales.pojo.vo.SpuVo;
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
            throw new GracefulResponseException("商品SPU不存在！");
        }

        spu.setVisible(form.getVisible());
        if (!iSpuRepository.updateById(spu)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public PageResult<ClientSpuVo> listClientSpu(ConditionQuery query, Integer categoryId) {
        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(categoryId))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }

        PageResult<ClientSpuVo> result = new PageResult<>();
        result.setTotal(iSpuRepository.count(new QueryWrapper<Spu>()
                .eq("category_id", categoryId)
                .eq("visible", true)));

        iSpuRepository.page(new Page<>(query.getCurrent(), query.getLimit()), new QueryWrapper<Spu>()
                .eq("category_id", categoryId)
                .eq("visible", true)).getRecords().forEach(spu -> {
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
    public PageResult<SpuVo> listSpu(ConditionQuery query, Integer categoryId) {
        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(categoryId))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }

        QueryWrapper<Spu> wrapper = QueryUtil.buildWrapper(query, Spu.class).eq("category_id", categoryId);
        PageResult<SpuVo> result = new PageResult<>();
        result.setTotal(iSpuRepository.count(wrapper));

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
