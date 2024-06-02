package com.mi.aftersales.service;

import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.ManngerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
import com.mi.aftersales.pojo.vo.MaterialVo;

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
    void addMaterial(MaterialFormVo form);

    /**
     * 更新物料信息。
     *
     * @param form 更新物料表单
     */
    void updateMaterial(ManngerUpdateMaterialFormVo form);

    MaterialVo getMaterialDetailById(String materialId);

    PageResult<MaterialVo> conditionQuery(ConditionQuery query);

    void deleteMaterialById(String materialId);
}
