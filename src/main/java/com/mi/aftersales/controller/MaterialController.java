package com.mi.aftersales.controller;

import com.mi.aftersales.service.MaterialService;
import com.mi.aftersales.vo.form.MaterialForm;
import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import io.swagger.v3.oas.annotations.Operation;
import org.redisson.api.RedissonClient;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 物料 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/material")
public class MaterialController {
    @Resource
    private MaterialService materialService;

    @Resource
    private RedissonClient redissonClient;

    /**
     * 库管添加物料。
     *
     * @param form 物料表单
     */
    @PostMapping(path = "/")
    @Operation(summary = "库管添加物料", description = "库管添加物料")
    public void addMaterial(@RequestBody @Valid MaterialForm form) {
        materialService.addMaterial(form);
    }

    /**
     * 库管更新物料信息。
     *
     * @param form 更新物料表单
     */
    @PutMapping(path = "/")
    @Operation(summary = "库管更新物料信息", description = "库管更新物料信息")
    public void updateMaterial(@RequestBody @Valid ManngerUpdateMaterialForm form) {
        materialService.updateMaterial(form, redissonClient);
    }
}
