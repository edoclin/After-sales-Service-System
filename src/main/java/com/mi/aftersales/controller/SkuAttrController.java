package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.service.SkuAttrService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.SkuAttrFormVo;
import com.mi.aftersales.pojo.vo.form.SkuAttrVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.ClientSkuAttrVo;
import com.mi.aftersales.pojo.vo.SkuAttrVo;
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
@RequestMapping("/aftersales/sku-attr")
public class SkuAttrController {
    @Resource
    private SkuAttrService skuAttrService;

    @PostMapping(path = "/manager")
    @Operation(summary = "管理员添加商品Sku属性", description = "管理员添加商品Sku属性")
    public void postSpu(@RequestBody @Valid SkuAttrFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        skuAttrService.addSkuAttr(form);
    }

    @PutMapping(path = "/manager/visible")
    @Operation(summary = "管理员更新商品Sku属性是否可见", description = "管理员更新商品Sku属性是否可见")
    public void putSpuVisible(@RequestBody @Valid SkuAttrVisibleSetFormVo form) {
        skuAttrService.updateSkuAttrVisibility(form);
    }

    @PostMapping(path = "/client/{skuId}")
    @Operation(summary = "客户查询商品Sku属性", description = "客户查询商品Sku属性")
    @Parameter(name = "skuId", description = "商品SkuId", required = true)
    public PageResult<ClientSkuAttrVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable String skuId) {
        return skuAttrService.listClientSkuAttrs(form, skuId);
    }

    @GetMapping(path = "/manager")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    @Parameter(name = "skuId", description = "商品SkuId", required = true)
    public PageResult<SkuAttrVo> list(@RequestBody ConditionQuery query) {
        return skuAttrService.listSkuAttrs(query);
    }
}
