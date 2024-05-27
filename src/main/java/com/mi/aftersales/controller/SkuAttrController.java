package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Sku;
import com.mi.aftersales.entity.SkuAttr;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.ISkuAttrService;
import com.mi.aftersales.service.ISkuService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SkuAttrForm;
import com.mi.aftersales.vo.form.SkuAttrVisibleSetForm;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
import com.mi.aftersales.vo.result.ClientSkuAttrVo;
import com.mi.aftersales.vo.result.ClientSkuVo;
import com.mi.aftersales.vo.result.SkuAttrVo;
import com.mi.aftersales.vo.result.SkuVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * sku属性 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/skuAttr")
public class SkuAttrController {
    @Resource
    private ISkuAttrService iSkuAttrService;
    @Resource
    private ISkuService iSkuService;


    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品SKU属性", description = "管理员添加商品SKU属性")
    public void postSpu(@RequestBody @Valid SkuAttrForm form) {
        if (BeanUtil.isEmpty(iSkuService.getById(form.getSkuId()))) {
            throw new GracefulResponseException("商品SKU不存在！");
        }

        SkuAttr skuAttr = new SkuAttr();
        BeanUtil.copyProperties(form, skuAttr);
        try {
            iSkuAttrService.save(skuAttr);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品SKU属性名称重复！");
        }
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品Sku属性是否可见", description = "管理员更新商品Sku属性是否可见")
    public void putSpuVisible(@RequestBody @Valid SkuAttrVisibleSetForm form) {
        SkuAttr skuAttr = iSkuAttrService.getById(form.getAttrId());
        if (BeanUtil.isEmpty(skuAttr)) {
            throw new GracefulResponseException("商品SKU属性不存在！");
        }

        skuAttr.setVisible(form.getVisible());

        if (!iSkuAttrService.updateById(skuAttr)) {
            throw new ServerErrorException();
        }
    }

    @PostMapping(path = "/client/{skuId}")
    @Operation(summary = "客户查询商品Sku属性", description = "客户查询商品Sku属性")
    @Parameter(name = "skuId", description = "商品SkuID", required = true)
    public PageResult<ClientSkuAttrVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String skuId) {
        if (BeanUtil.isEmpty(iSkuService.getById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        PageResult<ClientSkuAttrVo> result = new PageResult<>();
        result.setTotal(iSkuAttrService.lambdaQuery().eq(SkuAttr::getSkuId, skuId).eq(SkuAttr::getVisible, Boolean.TRUE).count());
        iSkuAttrService.lambdaQuery().eq(SkuAttr::getSkuId, skuId).eq(SkuAttr::getVisible, Boolean.TRUE).page(new Page<>(form.getCurrent(), form.getLimit())).getRecords().forEach(skuAttr -> {
            ClientSkuAttrVo clientSkuAttrVo = new ClientSkuAttrVo();
            BeanUtil.copyProperties(skuAttr, clientSkuAttrVo);
            result.getData().add(clientSkuAttrVo);
        });
        return result;
    }

    @PostMapping(path = "/list/{skuId}")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    @Parameter(name = "skuId", description = "商品SkuID", required = true)
    public PageResult<SkuAttrVo> list(@RequestBody ConditionQuery query, @PathVariable String skuId) {
        if (BeanUtil.isEmpty(iSkuAttrService.getById(skuId))) {
            throw new GracefulResponseException("商品Sku不存在！");
        }

        QueryWrapper<SkuAttr> wrapper = QueryUtil.buildWrapper(query, SkuAttr.class).eq("sku_id", skuId);

        PageResult<SkuAttrVo> result = new PageResult<>();
        result.setTotal(iSkuAttrService.count(wrapper));

        iSkuAttrService.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(skuAttr -> {
            SkuAttrVo skuAttrVo = new SkuAttrVo();
            BeanUtil.copyProperties(skuAttr, skuAttrVo, DateUtil.copyDate2yyyyMMddHHmm());
            result.getData().add(skuAttrVo);
        });
        return result;
    }

    @PostMapping(path = "/visible")
    @Operation(summary = "设置商品Sku是否可见（管理员）", description = "设置商品Sku是否可见（管理员）")
    public void list(@RequestBody @Valid SkuAttrVisibleSetForm form) {
        SkuAttr skuAttr = iSkuAttrService.getById(form.getAttrId());
        if (BeanUtil.isEmpty(skuAttr)) {
            throw new GracefulResponseException("商品SKU属性不存在！");
        }
        skuAttr.setVisible(form.getVisible());

        if (!iSkuAttrService.updateById(skuAttr)) {
            throw new ServerErrorException();
        }
    }
}
