package com.mi.aftersales.controller;

import com.mi.aftersales.service.iservice.ISpuService;
import com.mi.aftersales.vo.result.ClientSpuVo;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.result.SpuVo;
import com.mi.aftersales.vo.form.SpuForm;
import com.mi.aftersales.vo.form.UpdateSpuVisibleForm;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 商品 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/spu")
public class SpuController {
    @Resource
    private ISpuService iSpuService;

    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品", description = "管理员添加商品")
    public void postSpu(@RequestBody @Valid SpuForm form) {
        iSpuService.addSpu(form);
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品是否可见", description = "管理员更新商品是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSpuVisibleForm form) {
        iSpuService.updateSpuVisibility(form);
    }

    @PostMapping(path = "/client/{categoryId}")
    @Operation(summary = "客户查询商品", description = "客户查询商品")
    @Parameter(name = "categoryId", description = "所属分类", example = "1", required = true)
    public PageResult<ClientSpuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable Integer categoryId) {
        return iSpuService.listClientSpu(form, categoryId);
    }

    @PostMapping(path = "/list/{categoryId}")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    public PageResult<SpuVo> list(@RequestBody ConditionQuery query, @PathVariable Integer categoryId) {
        return iSpuService.listSpu(query, categoryId);
    }
}
