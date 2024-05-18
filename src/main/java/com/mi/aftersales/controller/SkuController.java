package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IFileService;
import com.mi.aftersales.service.ISkuService;
import com.mi.aftersales.service.ISpuService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.result.ClientSkuVo;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.SkuVo;
import com.mi.aftersales.vo.form.SkuForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 商品销售单元 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/sku")
public class SkuController {
    @Resource
    private ISkuService iSkuService;

    @Resource
    private ISpuService iSpuService;

    @Resource
    private IFileService iFileService;


    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品SKU", description = "管理员添加商品SKU")
    public void postSpu(@RequestBody @Valid SkuForm form) {

        if (BeanUtil.isEmpty(iSpuService.getById(form.getSpuId()))) {
            throw new GracefulResponseException("商品SPU不存在！");
        }

        if (BeanUtil.isEmpty(iFileService.getById(form.getSkuCoverFileId()))) {
            throw new GracefulResponseException("商品SKU封面图片不存在！");
        }
        Sku sku = new Sku();
        BeanUtil.copyProperties(form, sku);
        try {
            iSkuService.save(sku);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU名称重复！");
        }
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品Sku是否可见", description = "管理员更新商品Sku是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSkuVisibleForm form) {
        Sku byId = iSkuService.getById(form.getSkuId());
        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        byId.setVisible(form.getVisible());

        if (!iSkuService.updateById(byId)) {
            throw new ServerErrorException();
        }
    }

    @PostMapping(path = "/client/{spuId}")
    @Operation(summary = "客户查询商品Sku", description = "客户查询商品Sku")
    @Parameter(name = "spuId", description = "商品所属SpuID", example = "", required = true)
    public PageResult<ClientSkuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String spuId) {
        if (BeanUtil.isEmpty(iSpuService.getById(spuId))) {
            throw new GracefulResponseException("商品Spu不存在！");
        }

        PageResult<ClientSkuVo> result = new PageResult<>();
        result.setTotal(iSkuService.lambdaQuery().eq(Sku::getSpuId, spuId).eq(Sku::getVisible, Boolean.TRUE).count());
        iSkuService.lambdaQuery().eq(Sku::getSpuId, spuId).eq(Sku::getVisible, Boolean.TRUE).page(new Page<>(form.getCurrent(), form.getLimit())).getRecords().forEach(sku -> {
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

    @PostMapping(path = "/list/{spuId}")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    public PageResult<SkuVo> list(@RequestBody ConditionQuery query, @PathVariable String spuId) {
        if (BeanUtil.isEmpty(iSpuService.getById(spuId))) {
            throw new GracefulResponseException("商品所属Spu不存在！");
        }

        QueryWrapper<Sku> wrapper = QueryUtil.buildWrapper(query, Sku.class).eq("spu_id", spuId);

        PageResult<SkuVo> result = new PageResult<>();
        result.setTotal(iSkuService.count(wrapper));

        iSkuService.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(sku -> {
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

    @PostMapping(path = "/visible")
    @Operation(summary = "设置商品Sku是否可见（管理员）", description = "设置商品Sku是否可见（管理员）")
    public void list(@RequestBody @Valid SkuVisibleSetForm form) {
        Sku byId = iSkuService.getById(form.getSkuId());
        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        byId.setVisible(form.getVisible());
        if (!iSkuService.updateById(byId)) {
            throw new ServerErrorException();
        }

    }
}
