package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.ClientSkuVo;
import com.mi.aftersales.pojo.vo.SkuVo;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
import com.mi.aftersales.service.SkuService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
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

    @PostMapping(path = "/manager")
    @Operation(summary = "管理员添加商品Sku", description = "管理员添加商品Sku")
    public void postSpu(@RequestBody @Valid SkuFormVo form) {
        skuService.addSku(form);
    }

    @PutMapping(path = "/manager")
    @Operation(summary = "管理员更新商品Sku", description = "管理员更新商品Sku")
    public void postSpu(@RequestBody @Valid UpdateSkuFormVo form) {
        skuService.updateSkuById(form);
    }

    @PutMapping(path = "/manager/visible")
    @Operation(summary = "管理员更新商品Sku是否可见", description = "管理员更新商品Sku是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSkuVisibleFormVo form) {
        skuService.updateSkuVisibility(form);
    }

    @PostMapping(path = "/client/{spuId}")
    @Operation(summary = "客户查询商品Sku", description = "客户查询商品Sku")
    public PageResult<ClientSkuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String spuId) {
        return skuService.listClientSku(form, spuId);
    }

    @PostMapping(path = "/manager")
    @Operation(summary = "查询商品Sku（管理员）", description = "查询商品Sku（管理员）")
    public PageResult<SkuVo> conditionList(@RequestBody ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        return skuService.conditionList(query);
    }

    @DeleteMapping(path = "/manager/{skuId}")
    @Operation(summary = "删除商品Sku（管理员）", description = "删除商品Sku（管理员）")
    public void deleteSkuById(@PathVariable String skuId) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        skuService.removeSkuById(skuId);
    }
}
