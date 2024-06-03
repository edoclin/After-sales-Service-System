package com.mi.aftersales.controller;

import com.mi.aftersales.service.ClientServiceCenterService;
import com.mi.aftersales.pojo.vo.form.ClientServiceCenterFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateClientServiceCenterFormVo;
import io.swagger.v3.oas.annotations.Operation;
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
    private ClientServiceCenterService clientServiceCenterService;

    @PostMapping(path = "/")
    @Operation(summary = "添加用户服务中心", description = "添加用户服务中心")
    public void postClientServiceCenter(@RequestBody @Valid ClientServiceCenterFormVo form) {
        clientServiceCenterService.addClientServiceCenter(form);
    }

    /**
     * 修改用户服务中心。
     *
     * @param form 更新用户服务中心表单
     */
    @PutMapping(path = "/")
    @Operation(summary = "修改用户服务中心", description = "修改用户服务中心")
    public void putClientServiceCenter(@RequestBody @Valid UpdateClientServiceCenterFormVo form) {
        clientServiceCenterService.updateClientServiceCenter(form);
    }
}
