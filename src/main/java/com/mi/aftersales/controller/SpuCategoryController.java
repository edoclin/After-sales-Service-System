package com.mi.aftersales.controller;

import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.SpuCategory4ClientVo;
import com.mi.aftersales.pojo.vo.SpuCategoryVo4Manager;
import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuCategoryFormVo;
import com.mi.aftersales.service.SpuCategoryService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 商品分类 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/spu-category")
public class SpuCategoryController {
    @Resource
    private SpuCategoryService spuCategoryService;

    @PostMapping(path = "/manager")
    @Operation(summary = "Spu分类添加", description = "Spu分类添加")
    public void postSpuCategory(@RequestBody @Valid SpuCategoryFormVo form) {
        spuCategoryService.addSpuCategory(form);
    }


    @PutMapping(path = "/manager")
    @Operation(summary = "Spu分类更新", description = "Spu分类更新")
    public void poutSpuCategory(@RequestBody @Valid UpdateSpuCategoryFormVo form) {
        spuCategoryService.updateSpuCategory(form);
    }

    @GetMapping(path = "/manager")
    @Operation(summary = "Spu分类目录（管理员）", description = "Spu分类目录（管理员）")
    public PageResult<SpuCategoryVo4Manager> listSpuCategoryByCondition(@RequestBody @Valid ConditionQuery query) {
        return spuCategoryService.listSpuCategory(query);
    }


    @GetMapping(path = "/client")
    @Operation(summary = "Spu分类目录", description = "Spu分类目录")
    public List<SpuCategory4ClientVo> listSpuCategory4Client() {
        return spuCategoryService.listSpuCategory4Client(0);
    }


    @DeleteMapping(path = "/manager/{categoryId}")
    @Operation(summary = "删除Spu分类", description = "删除Spu分类")
    public void deleteSpuCategoryById(@PathVariable("categoryId") Integer categoryId) {
        spuCategoryService.deleteSpuCategoryById(categoryId);
    }

    @PutMapping(path = "/manager/visible")
    @Operation(summary = "Spu分类客户是否可见", description = "Spu分类客户是否可见")
    public void setVisible(@RequestBody @Valid SpuCategoryVisibleSetFormVo form) {
        spuCategoryService.setSpuCategoryVisibility(form);
    }
}
