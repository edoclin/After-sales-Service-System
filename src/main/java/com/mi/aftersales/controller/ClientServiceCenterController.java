package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.ClientServiceCenter;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.iservice.IClientServiceCenterService;
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
    @Resource
    private IClientServiceCenterService clientServiceCenterService;

    @PostMapping(path = "/")
    @Operation(summary = "添加用户服务中心", description = "添加用户服务中心")
    public void postClientServiceCenter(@RequestBody @Valid ClientServiceCenterForm form) {
        clientServiceCenterService.addClientServiceCenter(form);
    }

    /**
     * 修改用户服务中心。
     *
     * @param form 更新用户服务中心表单
     */
    @PutMapping(path = "/")
    @Operation(summary = "修改用户服务中心", description = "修改用户服务中心")
    public void putClientServiceCenter(@RequestBody @Valid UpdateClientServiceCenterForm form) {
        clientServiceCenterService.updateClientServiceCenter(form);
    }
}
