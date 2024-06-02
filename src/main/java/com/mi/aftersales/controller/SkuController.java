package com.mi.aftersales.controller;

import com.mi.aftersales.service.SkuService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.vo.form.SkuVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuVo;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.SkuVo;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
    private SkuService skuService;

    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品SKU", description = "管理员添加商品SKU")
    public void postSpu(@RequestBody @Valid SkuFormVo form) {
        skuService.addSku(form);
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品Sku是否可见", description = "管理员更新商品Sku是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSkuVisibleFormVo form) {
        skuService.updateSkuVisibility(form);
    }

    @PostMapping(path = "/client/{spuId}")
    @Operation(summary = "客户查询商品Sku", description = "客户查询商品Sku")
    @Parameter(name = "spuId", description = "商品所属SpuID", required = true)
    public PageResult<ClientSkuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String spuId) {
        return skuService.listClientSku(form, spuId);
    }

    @PostMapping(path = "/list/{spuId}")
    @Operation(summary = "查询商品Sku（管理员）", description = "查询商品Sku（管理员）")
    public PageResult<SkuVo> list(@RequestBody ConditionQuery query, @PathVariable String spuId) {
        return skuService.listSku(query, spuId);
    }

    @PostMapping(path = "/visible")
    @Operation(summary = "设置商品Sku是否可见（管理员）", description = "设置商品Sku是否可见（管理员）")
    public void list(@RequestBody @Valid SkuVisibleSetFormVo form) {
        skuService.setSkuVisibility(form);
    }
}
