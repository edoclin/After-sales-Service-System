package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import com.mi.aftersales.vo.form.MaterialForm;
import org.redisson.api.RedissonClient;

/**
 * <p>
 * 物料 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface MaterialService {
    String NAMESPACE_4_MATERIAL_LOCK = "material:lock:";
    /**
     * 添加物料。
     *
     * @param form 物料表单
     */
    void addMaterial(MaterialForm form);

    /**
     * 更新物料信息。
     *
     * @param form 更新物料表单
     * @param redissonClient Redisson 客户端
     */
    void updateMaterial(ManngerUpdateMaterialForm form, RedissonClient redissonClient);

}
