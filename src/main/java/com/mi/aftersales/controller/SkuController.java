package com.mi.aftersales.controller;

import com.mi.aftersales.service.ISkuService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.result.ClientSkuVo;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.vo.result.SkuVo;
import com.mi.aftersales.vo.form.SkuForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
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
    private ISkuService iSkuService;

    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品SKU", description = "管理员添加商品SKU")
    public void postSpu(@RequestBody @Valid SkuForm form) {
        iSkuService.addSku(form);
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品Sku是否可见", description = "管理员更新商品Sku是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSkuVisibleForm form) {
        iSkuService.updateSkuVisibility(form);
    }

    @PostMapping(path = "/client/{spuId}")
    @Operation(summary = "客户查询商品Sku", description = "客户查询商品Sku")
    @Parameter(name = "spuId", description = "商品所属SpuID", required = true)
    public PageResult<ClientSkuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String spuId) {
        return iSkuService.listClientSku(form, spuId);
    }

    @PostMapping(path = "/list/{spuId}")
    @Operation(summary = "查询商品Sku（管理员）", description = "查询商品Sku（管理员）")
    public PageResult<SkuVo> list(@RequestBody ConditionQuery query, @PathVariable String spuId) {
        return iSkuService.listSku(query, spuId);
    }

    @PostMapping(path = "/visible")
    @Operation(summary = "设置商品Sku是否可见（管理员）", description = "设置商品Sku是否可见（管理员）")
    public void list(@RequestBody @Valid SkuVisibleSetForm form) {
        iSkuService.setSkuVisibility(form);
    }
}
