package com.mi.aftersales.controller;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.convert.ConvertException;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IMaterialService;
import com.mi.aftersales.vo.form.MaterialForm;
import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import io.swagger.v3.oas.annotations.Operation;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.service.IMaterialService.NAMESPACE_4_MATERIAL_LOCK;

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
    private static final Logger log = LoggerFactory.getLogger(MaterialController.class);

    @Resource
    private IMaterialService imaterialService;

    @Resource
    private RedissonClient redissonClient;

    @PostMapping(path = "/")
    @Operation(summary = "库管添加物料", description = "库管添加物料")
    public void addMaterial(@RequestBody @Valid MaterialForm form) {
        // update 不用查询，直接save，有联合唯一索引，catch DuplicateKeyException异常
        if (form.getAlertNum().compareTo(form.getStock()) > 0) {
            throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
        }

        if (form.getCost().compareTo(form.getPrice()) > 0) {
            throw new GracefulResponseException("物料成本不能大于销售价格!");
        }

        Material material = new Material();

        try {
            BeanUtil.copyProperties(form, material);
            if (Boolean.FALSE.equals(imaterialService.save(material))) {
                throw new ServerErrorException();
            }
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("物料名称重复！");
        } catch (Exception e) {
            throw new ServerErrorException();
        }
    }

    @PutMapping(path = "/")
    @Operation(summary = "库管更新物料信息", description = "库管更新物料信息")
    public void updateMaterial(@RequestBody @Valid ManngerUpdateMaterialForm form) {
        Material material = imaterialService.getById(form.getMaterialId());

        if (BeanUtil.isEmpty(material)) {
            throw new GracefulResponseException("非法的物料Id");
        }
        //加分布式锁
        RLock fairLock = null;
        try {
            fairLock = redissonClient.getFairLock(NAMESPACE_4_MATERIAL_LOCK + form.getMaterialId());
            if (fairLock.tryLock(30, TimeUnit.SECONDS)) {
                // update 直接忽略null值
                BeanUtil.copyProperties(form, material, CopyOptions.create().ignoreNullValue());

                if (material.getAlertNum().compareTo(material.getStock()) > 0) {
                    throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
                }
                if (material.getCost().compareTo(material.getPrice()) > 0) {
                    throw new GracefulResponseException("物料成本不能大于销售价格!");
                }
                if (Boolean.FALSE.equals(imaterialService.updateById(material))) {
                    throw new ServerErrorException();
                }
            }
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        } finally {
            if (null != fairLock) {
                fairLock.unlock();
            }
        }
    }
}
