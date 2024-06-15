package com.mi.aftersales.controller;

import com.mi.aftersales.pojo.vo.form.UpdateSpuFormVo;
import com.mi.aftersales.service.SpuService;
import com.mi.aftersales.pojo.vo.ClientSpuVo;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.vo.SpuVo;
import com.mi.aftersales.pojo.vo.form.SpuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuVisibleFormVo;
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
    private SpuService spuService;

    @PostMapping(path = "/manager")
    @Operation(summary = "管理员添加商品Spu", description = "管理员添加商品Spu")
    public void postSpu(@RequestBody @Valid SpuFormVo form) {
        spuService.addSpu(form);
    }

    @PutMapping(path = "/manager/visible")
    @Operation(summary = "管理员更新商品Spu是否可见", description = "管理员更新商品Spu是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSpuVisibleFormVo form) {
        spuService.updateSpuVisibility(form);
    }

    @PutMapping(path = "/manager")
    @Operation(summary = "管理员更新商品Spu信息", description = "管理员更新Spu商品信息")
    public void putSpuVisible(@RequestBody @Valid UpdateSpuFormVo form) {
        spuService.updateSpuById(form);
    }


    @DeleteMapping(path = "/manager/{spuId}")
    @Operation(summary = "管理员删除商品Spu信息", description = "管理员删除商品Spu信息")
    public void deleteSpuById(@PathVariable String spuId) {
        spuService.removeSpuById(spuId);
    }

    @PostMapping(path = "/client/{categoryId}")
    @Operation(summary = "客户查询商品Spu（by 分类Id）", description = "客户查询商品Spu（by 分类Id）")
    @Parameter(name = "categoryId", description = "所属分类", example = "1", required = true)
    public PageResult<ClientSpuVo> list4Client(@RequestBody @Valid ConditionQuery query, @PathVariable Integer categoryId) {
        return spuService.listClientSpu(query, categoryId);
    }

    @PostMapping(path = "/query")
    @Operation(summary = "查询商品Spu（管理员）", description = "查询商品Spu（管理员）")
    public PageResult<SpuVo> list(@RequestBody ConditionQuery query) {
        return spuService.listSpu(query);
    }
}
