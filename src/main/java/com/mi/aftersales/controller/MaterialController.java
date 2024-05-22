package com.mi.aftersales.controller;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.ConvertException;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IMaterialService;
import com.mi.aftersales.vo.form.ManngerAddMaterialForm;
import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import io.swagger.v3.oas.annotations.Operation;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

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

    private static final String NAMESPACE_4_ORDER_LOCK = "materialLock:";

    @Resource
    private IMaterialService imaterialService;

    @Resource
    private RedissonClient redissonClient;

    @PostMapping(path = "/materialManager/add")
    @Operation(summary = "库管添加物料", description = "库管添加物料")
    public void addMaterial(@RequestBody @Valid ManngerAddMaterialForm form) {

        LambdaQueryWrapper<Material> queryWrapper = new LambdaQueryWrapper<>();

        queryWrapper.eq(null != form.getMaterialName(), Material::getMaterialName, form.getMaterialName());

        List<Material> materialList = imaterialService.list(queryWrapper);

        if (null != materialList && materialList.size() > 0) {
            throw new GracefulResponseException("物料命名重复，请更换物料名!");
        }

        if (form.getAlertNum().compareTo(form.getStock()) > 0) {
            throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
        }

        if (form.getCost().compareTo(form.getPrice()) > 0) {
            throw new GracefulResponseException("物料成本不能大于销售价格!");
        }


        Material material = new Material();

        try {
            BeanUtil.copyProperties(form, material);
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        }

        if (Boolean.FALSE.equals(imaterialService.save(material))) {
            throw new ServerErrorException();
        }
    }


    @PutMapping(path = "/materialManager/update")
    @Operation(summary = "库管更新物料信息", description = "库管更新物料信息")
    public void updateMaterial(@RequestBody @Valid ManngerUpdateMaterialForm form) {

        Material material = imaterialService.getById(form.getMaterialId());

        if (null != material) {
            //加分布式锁
            String lockKey = NAMESPACE_4_ORDER_LOCK + form.getMaterialId();
            RLock fairLock = redissonClient.getFairLock(lockKey);
            if (fairLock.tryLock()) {
                try {
                    if (form.getMaterialName() != null) {
                        material.setMaterialName(form.getMaterialName());
                    }
                    if (form.getMaterialDesc() != null) {
                        material.setMaterialDesc(form.getMaterialDesc());
                    }
                    if (form.getMaterialCoverFileId() != null) {
                        material.setMaterialCoverFileId(form.getMaterialCoverFileId());
                    }
                    if (form.getUnit() != null) {
                        material.setUnit(form.getUnit());
                    }
                    if (form.getStock() != null) {
                        material.setStock(material.getStock().add(form.getStock()));
                    }
                    if (form.getCost() != null) {
                        material.setCost(form.getCost());
                    }
                    if (form.getPrice() != null) {
                        material.setPrice(form.getPrice());
                    }
                    if (form.getAlertNum() != null) {
                        material.setAlertNum(form.getAlertNum());
                    }


                    if (material.getAlertNum().compareTo(material.getStock()) > 0) {
                        throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
                    }
                    if (material.getCost().compareTo(material.getPrice()) > 0) {
                        throw new GracefulResponseException("物料成本不能大于销售价格!");
                    }
                    if (Boolean.FALSE.equals(imaterialService.updateById(material))) {
                        throw new ServerErrorException();
                    }
                } catch (ConvertException e) {
                    throw new GracefulResponseException("物料类型不合法!");
                }
                fairLock.unlock();
            }

        } else {
            throw new GracefulResponseException("库存中不存在该物料!");
        }

    }
}
