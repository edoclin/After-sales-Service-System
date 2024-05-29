package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.ClientServiceCenterForm;
import com.mi.aftersales.vo.form.UpdateClientServiceCenterForm;

/**
 * <p>
 * 客户服务中心 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface ClientServiceCenterService {
    /**
     * 添加用户服务中心。
     *
     * @param form 用户服务中心表单
     */
    void addClientServiceCenter(ClientServiceCenterForm form);

    /**
     * 修改用户服务中心。
     *
     * @param form 更新用户服务中心表单
     */
    void updateClientServiceCenter(UpdateClientServiceCenterForm form);

}
