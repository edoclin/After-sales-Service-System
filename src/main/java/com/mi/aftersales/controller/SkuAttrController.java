package com.mi.aftersales.controller;

import com.mi.aftersales.service.SkuAttrService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.form.SkuAttrForm;
import com.mi.aftersales.vo.form.SkuAttrVisibleSetForm;
import com.mi.aftersales.vo.result.ClientSkuAttrVo;
import com.mi.aftersales.vo.result.SkuAttrVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
    private SkuAttrService skuAttrService;

    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品SKU属性", description = "管理员添加商品SKU属性")
    public void postSpu(@RequestBody @Valid SkuAttrForm form) {
        skuAttrService.addSkuAttr(form);
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品Sku属性是否可见", description = "管理员更新商品Sku属性是否可见")
    public void putSpuVisible(@RequestBody @Valid SkuAttrVisibleSetForm form) {
        skuAttrService.updateSkuAttrVisibility(form);
    }

    @PostMapping(path = "/client/{skuId}")
    @Operation(summary = "客户查询商品Sku属性", description = "客户查询商品Sku属性")
    @Parameter(name = "skuId", description = "商品SkuID", required = true)
    public PageResult<ClientSkuAttrVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String skuId) {
        return skuAttrService.listClientSkuAttrs(form, skuId);
    }

    @PostMapping(path = "/list/{skuId}")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    @Parameter(name = "skuId", description = "商品SkuID", required = true)
    public PageResult<SkuAttrVo> list(@RequestBody ConditionQuery query, @PathVariable String skuId) {
        return skuAttrService.listSkuAttrs(query, skuId);
    }
}
