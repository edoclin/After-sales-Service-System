package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IClientServiceCenterService;
import com.mi.aftersales.vo.form.ClientServiceCenterForm;
import com.mi.aftersales.vo.form.UpdateClientServiceCenterForm;
import io.swagger.v3.oas.annotations.Operation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 客户服务中心 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/clientServiceCenter")
public class ClientServiceCenterController {
    private static final Logger log = LoggerFactory.getLogger(ClientServiceCenterController.class);
    @Resource
    private IClientServiceCenterService clientServiceCenterService;

    @PostMapping(path = "/")
    @Operation(summary = "添加用户服务中心", description = "添加用户服务中心")
    public void postClientServiceCenter(@RequestBody @Valid ClientServiceCenterForm form) {
        ClientServiceCenter clientServiceCenter = new ClientServiceCenter();
        BeanUtil.copyProperties(form, clientServiceCenter);
        try {
            clientServiceCenterService.save(clientServiceCenter);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @PutMapping(path = "/")
    @Operation(summary = "修改用户服务中心", description = "修改用户服务中心")
    public void putClientServiceCenter(@RequestBody @Valid UpdateClientServiceCenterForm form) {
        ClientServiceCenter byId = clientServiceCenterService.getById(form.getCenterId());

        if (BeanUtil.isNotEmpty(byId)) {
            BeanUtil.copyProperties(form, byId);
            try {
                clientServiceCenterService.updateById(byId);
            } catch (Exception e) {
                log.error(e.getMessage());
                throw new ServerErrorException();
            }
        } else {
            throw new GracefulResponseException(CharSequenceUtil.format("指定服务中心（ID：{}）不存在", form.getCenterId()));
        }
    }
}
