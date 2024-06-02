package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.ClientServiceCenterService;
import com.mi.aftersales.repository.IClientServiceCenterRepository;
import com.mi.aftersales.pojo.vo.form.ClientServiceCenterFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateClientServiceCenterFormVo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

    private static final Logger log = LoggerFactory.getLogger(ClientServiceCenterServiceImpl.class);
    @Resource
    private IClientServiceCenterRepository iClientServiceCenterRepository;

    @Override
    public void addClientServiceCenter(ClientServiceCenterFormVo form) {
        ClientServiceCenter clientServiceCenter = new ClientServiceCenter();
        BeanUtil.copyProperties(form, clientServiceCenter);
        try {
            iClientServiceCenterRepository.save(clientServiceCenter);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public void updateClientServiceCenter(UpdateClientServiceCenterFormVo form) {
        ClientServiceCenter byId = iClientServiceCenterRepository.getById(form.getCenterId());

        if (BeanUtil.isNotEmpty(byId)) {
            BeanUtil.copyProperties(form, byId);
            try {
                iClientServiceCenterRepository.updateById(byId);
            } catch (Exception e) {
                log.error(e.getMessage());
                throw new ServerErrorException();
            }
        } else {
            throw new GracefulResponseException(CharSequenceUtil.format("指定服务中心（ID：{}）不存在", form.getCenterId()));
        }
    }
}
