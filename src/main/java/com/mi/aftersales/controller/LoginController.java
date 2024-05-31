package com.mi.aftersales.controller;

import cn.dev33.satoken.session.SaSession;
import cn.dev33.satoken.stp.StpUtil;
import cn.dev33.satoken.temp.SaTempUtil;
import cn.hutool.captcha.generator.RandomGenerator;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.CheckLoginAspect;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.config.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.controller.enums.SmsCodeType;
import com.mi.aftersales.repository.*;
import com.mi.aftersales.service.LoginService;
import com.mi.aftersales.vo.form.LoginBindForm;
import com.mi.aftersales.vo.form.LoginBySmsForm;
import com.mi.aftersales.vo.form.SendSmsCodeForm;
import com.mi.aftersales.vo.result.SmsResultVo;
import com.mi.aftersales.vo.result.ThirdLoginPageResultVo;
import com.mi.aftersales.vo.result.LoginResultVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import me.zhyd.oauth.config.AuthConfig;
import me.zhyd.oauth.model.AuthCallback;
import me.zhyd.oauth.model.AuthResponse;
import me.zhyd.oauth.model.AuthUser;
import me.zhyd.oauth.request.AuthGithubRequest;
import me.zhyd.oauth.request.AuthMiRequest;
import me.zhyd.oauth.request.AuthRequest;
import me.zhyd.oauth.utils.AuthStateUtils;
import org.dromara.sms4j.api.SmsBlend;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.dromara.sms4j.core.factory.SmsFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

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

    /**
     * @description: 三方登录成功状态码
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 16:35
     **/
    @Resource
    private LoginService loginService;

    @GetMapping(path = "/check")
    @Operation(summary = "检查是否登录", description = "检查是否登录")
    @CheckLogin
    public void checkLogin() {
        loginService.checkLogin();
    }

    @GetMapping(path = "/logout")
    @Operation(summary = "退出登录", description = "退出登录")
    @CheckLogin
    public void logout() {
        loginService.logout();
    }

    @PostMapping(path = "/sms/send")
    @Operation(summary = "发送短信验证码", description = "发送短信验证码")
    public SmsResultVo sendSmsCode(@RequestBody @Valid SendSmsCodeForm form) {
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
    public LoginResultVo bind(@RequestBody @Valid LoginBindForm form) {
        return loginService.bind(form);
    }


    @PostMapping(path = "/sms")
    @Operation(summary = "手机验证码登录", description = "手机验证码登录")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "autoRegister", description = "未注册用户是否自动注册", example = "true", required = true)
    public LoginResultVo loginBySms(@RequestBody @Valid LoginBySmsForm form) {
        return loginService.loginBySms(form);
    }
}
