package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.FapiaoService;
import com.mi.aftersales.vo.result.ClientFapiaoVo;
import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

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
    private FapiaoService fapiaoService;

    @PostMapping(path = "/")
    @CheckLogin
    @Operation(summary = "添加发票", description = "添加发票")
    public void postFapiao(@RequestBody @Valid FapiaoForm form) {
        fapiaoService.addFapiao(form);
    }

    @GetMapping(path = "/client")
    @CheckLogin
    @Operation(summary = "查询当前登录用户发票", description = "查询当前登录用户发票")
    public List<ClientFapiaoVo> listFapiao() {
        return fapiaoService.listFapiaoByClient(StpUtil.getLoginIdAsString());
    }

    @DeleteMapping(path = "/client/{fapiaoId}")
    @CheckLogin
    @Operation(summary = "当前用户删除发票信息", description = "当前用户删除发票信息")
    public void delFapiao(@PathVariable String fapiaoId) {
        fapiaoService.deleteFapiaoByClient(fapiaoId, StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client")
    @CheckLogin
    @Operation(summary = "当前用户修改发票信息", description = "当前用户修改发票信息")
    public void updateFapiao(@RequestBody @Valid UpdateFapiaoForm form) {
        fapiaoService.updateFapiaoByClient(form, StpUtil.getLoginIdAsString());
    }
}
