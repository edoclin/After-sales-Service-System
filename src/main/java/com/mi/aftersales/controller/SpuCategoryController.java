package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckPermission;
import com.mi.aftersales.service.iservice.ISpuCategoryService;
import com.mi.aftersales.vo.result.SpuCategory4ClientVo;
import com.mi.aftersales.vo.form.SpuCategoryForm;
import com.mi.aftersales.vo.form.SpuCategoryVisibleSetForm;
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
    private ISpuCategoryService iSpuCategoryService;

    @PostMapping(path = "/")
    @CheckPermission
    @Operation(summary = "SPU分类添加", description = "SPU分类添加")
    public void postSpuCategory(@RequestBody @Valid SpuCategoryForm form) {
        iSpuCategoryService.addSpuCategory(form);
    }

    @GetMapping(path = "/list/client")
    @Operation(summary = "SPU分类目录", description = "SPU分类目录")
    public List<SpuCategory4ClientVo> listSpuCategory4Client() {
        return iSpuCategoryService.listSpuCategory4Client(0);
    }

    @PutMapping(path = "/visible")
    @CheckPermission
    @Operation(summary = "SPU分类客户是否可见", description = "SPU分类客户是否可见")
    public void setVisible(@RequestBody @Valid SpuCategoryVisibleSetForm form) {
        iSpuCategoryService.setSpuCategoryVisibility(form);
    }
}
