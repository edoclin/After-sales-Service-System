package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.Material;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 物料 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface IMaterialService extends IService<Material> {
    String NAMESPACE_4_MATERIAL_LOCK = "material:lock:";

}
