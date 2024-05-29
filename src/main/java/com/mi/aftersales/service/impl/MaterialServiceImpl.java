package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.convert.ConvertException;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.MaterialService;
import com.mi.aftersales.service.iservice.IMaterialService;
import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import com.mi.aftersales.vo.form.MaterialForm;
import lombok.SneakyThrows;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 * 物料 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class MaterialServiceImpl  implements MaterialService {
    @Resource
    private IMaterialService iMaterialService;

    @Override
    public void addMaterial(MaterialForm form) {
        if (form.getAlertNum().compareTo(form.getStock()) > 0) {
            throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
        }

        if (form.getCost().compareTo(form.getPrice()) > 0) {
            throw new GracefulResponseException("物料成本不能大于销售价格!");
        }

        Material material = new Material();
        try {
            BeanUtil.copyProperties(form, material);
            if (Boolean.FALSE.equals(iMaterialService.save(material))) {
                throw new ServerErrorException();
            }
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("物料名称重复！");
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @SneakyThrows
    @Override
    public void updateMaterial(ManngerUpdateMaterialForm form, RedissonClient redissonClient) {
        Material material = iMaterialService.getById(form.getMaterialId());

        if (BeanUtil.isEmpty(material)) {
            throw new GracefulResponseException("非法的物料Id");
        }

        RLock fairLock = null;
        try {
            fairLock = redissonClient.getFairLock(NAMESPACE_4_MATERIAL_LOCK + form.getMaterialId());
            if (fairLock.tryLock(30, TimeUnit.SECONDS)) {
                BeanUtil.copyProperties(form, material, CopyOptions.create().ignoreNullValue());

                if (material.getAlertNum().compareTo(material.getStock()) > 0) {
                    throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
                }
                if (material.getCost().compareTo(material.getPrice()) > 0) {
                    throw new GracefulResponseException("物料成本不能大于销售价格!");
                }
                if (Boolean.FALSE.equals(iMaterialService.updateById(material))) {
                    throw new ServerErrorException();
                }
            }
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (ServerErrorException e) {

        } finally {
            if (null != fairLock) {
                fairLock.unlock();
            }
        }
    }
}
