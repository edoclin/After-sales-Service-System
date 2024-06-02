package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckPermission;
import com.mi.aftersales.service.SpuCategoryService;
import com.mi.aftersales.pojo.vo.SpuCategory4ClientVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
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
@RequestMapping("/aftersales/spuCategory")
public class SpuCategoryController {
    @Resource
    private SpuCategoryService spuCategoryService;

    @PostMapping(path = "/")
    @CheckPermission
    @Operation(summary = "SPU分类添加", description = "SPU分类添加")
    public void postSpuCategory(@RequestBody @Valid SpuCategoryFormVo form) {
        spuCategoryService.addSpuCategory(form);
    }

    @GetMapping(path = "/list/client")
    @Operation(summary = "SPU分类目录", description = "SPU分类目录")
    public List<SpuCategory4ClientVo> listSpuCategory4Client() {
        return spuCategoryService.listSpuCategory4Client(0);
    }

    @PutMapping(path = "/visible")
    @CheckPermission
    @Operation(summary = "SPU分类客户是否可见", description = "SPU分类客户是否可见")
    public void setVisible(@RequestBody @Valid SpuCategoryVisibleSetFormVo form) {
        spuCategoryService.setSpuCategoryVisibility(form);
    }
}
