package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.service.LoginService;
import com.mi.aftersales.pojo.vo.form.LoginBindFormVo;
import com.mi.aftersales.pojo.vo.form.LoginBySmsFormVo;
import com.mi.aftersales.pojo.vo.form.SendSmsCodeFormVo;
import com.mi.aftersales.pojo.vo.SmsResultVo;
import com.mi.aftersales.pojo.vo.ThirdLoginPageResultVo;
import com.mi.aftersales.pojo.vo.LoginResultVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import me.zhyd.oauth.model.AuthCallback;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 登录表 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/login")
public class LoginController {
    private static final Logger log = LoggerFactory.getLogger(LoginController.class);


    @Resource
    private LoginService loginService;

    @GetMapping(path = "/check")
    @Operation(summary = "检查是否登录", description = "检查是否登录")
    @CheckLogin
    public void checkLogin() {
        // 根据返回值判断是否登录
        loginService.checkLogin();
    }

    @GetMapping(path = "/logout")
    @Operation(summary = "退出当前登录用户", description = "退出当前登录用户")
    @CheckLogin
    public void logout() {
        loginService.logout();
    }

    @PostMapping(path = "/sms/send")
    @Operation(summary = "发送短信验证码", description = "发送短信验证码")
    public SmsResultVo sendSmsCode(@RequestBody @Valid SendSmsCodeFormVo form) {
        return loginService.sendSmsCode(form);
    }


    @GetMapping(path = "/oauth2/{client}/render")
    @Operation(summary = "三方登录页面渲染", description = "三方登录页面渲染")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)
    public ThirdLoginPageResultVo githubRender(@PathVariable String client) {
        return loginService.githubRender(client);
    }


    @GetMapping(path = "/oauth2/{client}/callback")
    @Operation(summary = "三方登录回调接口", description = "三方登录回调接口")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)
    @Parameter(name = "callback", description = "回调参数", example = "github", required = true)
    public LoginResultVo callback(@PathVariable String client, AuthCallback callback) {
        return loginService.callback(client, callback);
    }

    @PostMapping(path = "/bind")
    @Operation(summary = "三方登录绑定手机号", description = "三方登录绑定手机号")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "tempToken", description = "临时令牌", example = "绑定所需的临时令牌，由callback接口提供", required = true)
    public LoginResultVo bind(@RequestBody @Valid LoginBindFormVo form) {
        return loginService.bind(form);
    }


    @PostMapping(path = "/sms")
    @Operation(summary = "手机验证码登录", description = "手机验证码登录")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "autoRegister", description = "开启自动注册", example = "true", required = true)
    public LoginResultVo loginBySms(@RequestBody @Valid LoginBySmsFormVo form) {
        return loginService.loginBySms(form);
    }
}
