package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.service.MaterialService;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
import com.mi.aftersales.pojo.vo.form.ManagerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.MaterialVo;
import io.swagger.v3.oas.annotations.Operation;
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


    /**
     * 库管添加物料。
     *
     * @param form 物料表单
     */
    @PostMapping(path = "/")
    @Operation(summary = "库管添加物料", description = "库管添加物料")
    @CheckLogin
    public void addMaterial(@RequestBody @Valid MaterialFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        materialService.addMaterial(form);
    }

    /**
     * 库管更新物料信息。
     *
     * @param form 更新物料表单
     */
    @PutMapping(path = "/")
    @Operation(summary = "库管更新物料信息", description = "库管更新物料信息")
    @CheckLogin
    public void updateMaterial(@RequestBody @Valid ManagerUpdateMaterialFormVo form) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        materialService.updateMaterial(form);
    }

    @GetMapping(path = "/{materialId}")
    @Operation(summary = "查询物料变动日志", description = "查询物料变动日志")
    @CheckLogin
    public MaterialVo getMaterialDetailById(@PathVariable String materialId) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        return materialService.getMaterialDetailById(materialId);
    }

    @PostMapping(path = "/condition")
    @Operation(summary = "查询物料", description = "查询物料")
    public PageResult<MaterialVo> conditionQuery(@RequestBody @Valid ConditionQuery query) {
        StpUtil.checkRoleOr(EmployeeRoleEnum.MATERIAL_MANAGER.name(), EmployeeRoleEnum.ENGINEER.name());
        return materialService.conditionQuery(query);
    }

    @DeleteMapping(path = "/manager/{materialId}")
    @Operation(summary = "删除物料", description = "删除物料")
    @CheckLogin
    public void deleteMaterialById(@PathVariable String materialId) {
        StpUtil.checkRole(EmployeeRoleEnum.MATERIAL_MANAGER.name());
        materialService.deleteMaterialById(materialId);
    }
}
