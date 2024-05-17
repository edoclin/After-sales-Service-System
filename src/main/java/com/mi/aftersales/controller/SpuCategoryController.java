package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.anno.CheckPermission;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.service.ISpuCategoryService;
import com.mi.aftersales.vo.result.SpuCategory4ClientVo;
import com.mi.aftersales.vo.form.SpuCategoryForm;
import com.mi.aftersales.vo.form.SpuCategorySetVisibleForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.dao.DuplicateKeyException;
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
        if (form.getParentCategoryId() != 0 && BeanUtil.isEmpty(iSpuCategoryService.getById(form.getParentCategoryId()))) {
            throw new GracefulResponseException("指定父级分类不存在");
        } else {
            SpuCategory spuCategory = new SpuCategory();
            BeanUtil.copyProperties(form, spuCategory);

            try {
                iSpuCategoryService.save(spuCategory);
            } catch (DuplicateKeyException e) {
                throw new GracefulResponseException("SPU分类名称已存在");
            }
        }
    }

    @GetMapping(path = "/list/client")
    @Operation(summary = "SPU分类目录", description = "SPU分类目录")
    public List<SpuCategory4ClientVo> listSpuCategory4Client() {
        return iSpuCategoryService.listSpuCategory4Client(0);
    }

    @GetMapping(path = "/visible")
    @CheckPermission
    @Operation(summary = "SPU分类客户是否可见", description = "SPU分类客户是否可见")
    public void setVisible(@RequestBody @Valid SpuCategorySetVisibleForm form) {
        SpuCategory byId = iSpuCategoryService.getById(form.getCategoryId());

        if (BeanUtil.isNotEmpty(byId)) {
            byId.setVisible(form.getVisible());
            iSpuCategoryService.updateById(byId);
        } else {
            throw new GracefulResponseException("分类ID不存在");
        }
    }
}
