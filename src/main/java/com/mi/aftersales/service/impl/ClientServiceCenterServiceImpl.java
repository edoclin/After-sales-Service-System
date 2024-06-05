package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.exception.graceful.IllegalClientServiceCenterIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.pojo.vo.form.ClientServiceCenterFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateClientServiceCenterFormVo;
import com.mi.aftersales.repository.IClientServiceCenterRepository;
import com.mi.aftersales.service.ClientServiceCenterService;
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
        ClientServiceCenter clientServiceCenter = iClientServiceCenterRepository.getById(form.getCenterId());

        if (BeanUtil.isEmpty(clientServiceCenter)) {
            throw new IllegalClientServiceCenterIdException();
        }
        BeanUtil.copyProperties(form, clientServiceCenter);
        try {
            iClientServiceCenterRepository.updateById(clientServiceCenter);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }
}
