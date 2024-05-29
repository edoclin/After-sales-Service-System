package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.Fapiao;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.iservice.IFapiaoService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.result.ClientFapiaoVo;
import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
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
    private IFapiaoService fapiaoService;

    @PostMapping(path = "/")
    @CheckLogin
    @Operation(summary = "添加发票", description = "添加发票")
    public void postFapiao(@RequestBody @Valid FapiaoForm form) {
        Fapiao fapiao = new Fapiao();
        BeanUtil.copyProperties(form, fapiao);
        try {
            fapiaoService.save(fapiao);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("发票号码已存在！");
        } catch (Exception e) {
            throw new ServerErrorException();
        }
    }

    @GetMapping(path = "/client")
    @CheckLogin
    @Operation(summary = "查询当前登录用户发票", description = "查询当前登录用户发票")
    public List<ClientFapiaoVo> listFapiao() {
        ArrayList<ClientFapiaoVo> result = new ArrayList<>();
        fapiaoService.lambdaQuery().eq(Fapiao::getCreatedId, StpUtil.getLoginIdAsString()).list().forEach(fapiao -> {
            ClientFapiaoVo item = new ClientFapiaoVo();
            BeanUtil.copyProperties(fapiao, item, DateUtil.copyDate2yyyyMMddHHmm());
            result.add(item);
        });
        return result;
    }

    @DeleteMapping(path = "/client/{fapiaoId}")
    @CheckLogin
    @Operation(summary = "当前用户删除发票信息", description = "当前用户删除发票信息")
    public void delFapiao(@PathVariable String fapiaoId) {
        Fapiao byId = fapiaoService.getById(fapiaoId);

        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("发票ID不存在！");
        } else {
            if (!CharSequenceUtil.equals(byId.getCreatedId(), StpUtil.getLoginIdAsString())) {
                throw new GracefulResponseException("该发票不属于当前用户！");
            } else {
                fapiaoService.removeById(fapiaoId);
            }
        }
    }

    @PutMapping(path = "/client")
    @CheckLogin
    @Operation(summary = "当前用户修改发票信息", description = "当前用户修改发票信息")
    public void updateFapiao(@RequestBody @Valid UpdateFapiaoForm form) {
        Fapiao byId = fapiaoService.getById(form.getFapiaoId());

        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("发票ID不存在！");
        } else {
            if (!CharSequenceUtil.equals(byId.getCreatedId(), StpUtil.getLoginIdAsString())) {
                throw new GracefulResponseException("该发票不属于当前用户！");
            } else {
                BeanUtil.copyProperties(form, byId, CopyOptions.create().ignoreNullValue());
                fapiaoService.updateById(byId);
            }
        }
    }
}
