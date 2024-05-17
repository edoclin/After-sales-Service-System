package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Fapiao;
import com.mi.aftersales.service.IFapiaoService;
import com.mi.aftersales.vo.form.FapiaoForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 发票信息 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/fapiao")
public class FapiaoController {

    @Resource
    private IFapiaoService fapiaoService;

    @PostMapping(path = "/")
    @Operation(summary = "添加发票", description = "添加发票")
    public void postFapiao(@RequestBody @Valid FapiaoForm form) {
        Fapiao fapiao = new Fapiao();
        BeanUtil.copyProperties(form, fapiao);
        if (!fapiaoService.save(fapiao)) {
            throw new GracefulResponseException("发票保存失败！");
        }
    }
}
