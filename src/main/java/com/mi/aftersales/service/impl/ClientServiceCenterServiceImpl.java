package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.ClientServiceCenterService;
import com.mi.aftersales.service.iservice.IClientServiceCenterService;
import com.mi.aftersales.vo.form.ClientServiceCenterForm;
import com.mi.aftersales.vo.form.UpdateClientServiceCenterForm;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <p>
 * 客户服务中心 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class ClientServiceCenterServiceImpl implements ClientServiceCenterService {
    @Resource
    private IClientServiceCenterService iClientServiceCenterService;

    @Override
    public void addClientServiceCenter(ClientServiceCenterForm form) {
        ClientServiceCenter clientServiceCenter = new ClientServiceCenter();
        BeanUtil.copyProperties(form, clientServiceCenter);
        try {
            iClientServiceCenterService.save(clientServiceCenter);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public void updateClientServiceCenter(UpdateClientServiceCenterForm form) {
        ClientServiceCenter byId = iClientServiceCenterService.getById(form.getCenterId());

        if (BeanUtil.isNotEmpty(byId)) {
            BeanUtil.copyProperties(form, byId);
            try {
                iClientServiceCenterService.updateById(byId);
            } catch (Exception e) {
                throw new ServerErrorException(e.getMessage());
            }
        } else {
            throw new GracefulResponseException(CharSequenceUtil.format("指定服务中心（ID：{}）不存在", form.getCenterId()));
        }
    }
}
