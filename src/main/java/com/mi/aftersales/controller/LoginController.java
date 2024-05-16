package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.dev33.satoken.temp.SaTempUtil;
import cn.hutool.captcha.generator.RandomGenerator;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.config.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.exception.graceful.*;
import com.mi.aftersales.service.ILoginService;
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
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
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

    @Resource
    private RedissonClient redissonClient;

    @Resource
    private StringRedisTemplate redisTemplate4Sms;


    @Resource
    private ILoginService iLoginService;

    /**
     * @description: 三方登录成功状态码
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 16:35
     **/
    private static final int OAUTH2_SUCCESS_CODE = 2000;
    @Resource
    private OAuthList oAuthList;

    @Resource
    private CustomSmsConfig customSmsConfig;

    @GetMapping(path = "/sms")
    public SmsResultVo sendSmsCode(@RequestBody @Valid SendSmsCodeForm form) {
        SmsResultVo smsResultVo = new SmsResultVo();
        RandomGenerator randomGenerator = new RandomGenerator("0123456789", 6);

        String key = "sms:" + form.getMobile();
        String smsCode = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(smsCode)) {
            // 不存在, 发送验证码
            String code = randomGenerator.generate() + "_" + DateUtil.currentSeconds();
            // todo send sms
            redisTemplate4Sms.opsForValue().set(key, code, customSmsConfig.getValidTime(), TimeUnit.SECONDS);
            smsResultVo.setSuccess(true);
        } else {
            // 存在, 判断发送时间
            long timestamp = Long.parseLong(smsCode.split("_")[1]);
            Long delta = DateUtil.currentSeconds() - timestamp;
            if (delta <= customSmsConfig.getPeriod()) {
                // 间隔时间小于周期
                smsResultVo.setSuccess(false);
                smsResultVo.setInfo(CharSequenceUtil.format("请{}秒后再尝试!", customSmsConfig.getPeriod() - delta));
            } else {
                // todo send sms
                String code = randomGenerator.generate() + "_" + DateUtil.currentSeconds();
                redisTemplate4Sms.opsForValue().set(key, code, customSmsConfig.getValidTime(), TimeUnit.SECONDS);
                smsResultVo.setSuccess(true);
            }
        }
        return smsResultVo;
    }


    @GetMapping(path = "/oauth2/{client}/render")
    @Operation(summary = "三方登录页面渲染", description = "三方登录页面渲染")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)
    public ThirdLoginPageResultVo githubRender(@PathVariable String client) {
        return new ThirdLoginPageResultVo().setUrl(getAuthRequest(client).authorize(AuthStateUtils.createState()));
    }


    @GetMapping("/oauth2/{client}/callback")
    @Operation(summary = "三方登录回调接口", description = "三方登录回调接口")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)
    @Parameter(name = "callback", description = "回调参数", example = "github", required = true)
    public LoginResultVo callback(@PathVariable String client, AuthCallback callback) {
        LoginResultVo loginResultVo = new LoginResultVo();
        AuthResponse authResponse = getAuthRequest(client).login(callback);
        if (authResponse.getCode() == OAUTH2_SUCCESS_CODE) {
            // 三方授权成功
            AuthUser data = (AuthUser) authResponse.getData();
            Login login = iLoginService.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getSource, LoginOAuthSourceEnum.valueOf(client.toUpperCase())).eq(Login::getAppId, data.getUuid()));
            if (BeanUtil.isEmpty(login)) {
                // 让用户绑定手机号
                loginResultVo.setNeedMobile(Boolean.TRUE);
                String tempToken = SaTempUtil.createToken(CharSequenceUtil.format("{}:{}", data.getSource(), data.getUuid()), 300);
                loginResultVo.setTempToken(tempToken);
            } else {
                StpUtil.login(login.getLoginId());
                loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
            }
        } else {
            log.error(authResponse.getMsg());
            throw new ThirdLoginFailedException();
        }
        return loginResultVo;
    }

    @PostMapping("/bind")
    @Operation(summary = "三方登录绑定手机号", description = "三方登录绑定手机号")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "tempToken", description = "临时令牌", example = "绑定所需的临时令牌，由callback接口提供", required = true)
    public LoginResultVo bind(@RequestBody @Valid LoginBindForm form) {
        LoginResultVo loginResultVo = new LoginResultVo();
        // 验证验证码是否正确
        String key = "sms:" + form.getMobile();
        String code = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(code)) {
            throw new SmsCodeTimeoutException();
        } else {
            code = code.split("_")[0];
            if (CharSequenceUtil.equals(form.getCode(), code)) {
                redisTemplate4Sms.delete(key);
                // 验证码正确
                String[] user;
                try {
                    // 验证tempToken是否有效
                    user = SaTempUtil.parseToken(form.getTempToken(), String.class).split(":");
                } catch (NullPointerException e) {
                    throw new IllegalTempTokenException();
                }
                SaTempUtil.deleteToken(form.getTempToken());
                Login login = iLoginService.lambdaQuery().eq(Login::getMobile, form.getMobile()).one();

                if (BeanUtil.isEmpty(login)) {
                    // 未注册用户
                    login = new Login();
                    login.setMobile(form.getMobile());
                    login.setLoginType(LoginTypeEnum.CLIENT);
                    login.setSource(LoginOAuthSourceEnum.valueOf(user[0].toUpperCase()));
                    login.setAppId(user[1]);
                    login.setLoginId(IdUtil.getSnowflakeNextIdStr());
                } else {
                    // 已使用手机号注册的用户绑定三方登录
                    login.setSource(LoginOAuthSourceEnum.valueOf(user[0].toUpperCase()));
                    login.setAppId(user[1]);
                }

                if (iLoginService.saveOrUpdate(login)) {
                    StpUtil.login(login.getLoginId());
                    loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                }

            } else {
                throw new IllegalSmsCodeException();
            }
        }
        return loginResultVo;
    }


    @PostMapping("/sms")
    @Operation(summary = "手机验证码登录", description = "手机验证码登录")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "autoRegister", description = "未注册用户是否自动注册", example = "true", required = true)
    public LoginResultVo login(@RequestBody @Valid LoginBySmsForm form) {
        LoginResultVo loginResultVo = new LoginResultVo();
        // 验证验证码是否正确
        String key = "sms:" + form.getMobile();
        String code = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(code)) {
            throw new SmsCodeTimeoutException();
        } else {
            code = code.split("_")[0];
            if (CharSequenceUtil.equals(form.getCode(), code)) {
                redisTemplate4Sms.delete(key);
                // 验证码正确
                Login login = iLoginService.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getMobile, form.getMobile()));
                if (BeanUtil.isNotEmpty(login)) {
                    StpUtil.login(login.getLoginId());
                    loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                } else {
                    if (Boolean.TRUE.equals(form.getAutoRegister())) {
                        // 自动注册
                        login = new Login();
                        login.setMobile(form.getMobile());
                        login.setLoginType(LoginTypeEnum.CLIENT);
                        login.setLoginId(IdUtil.getSnowflakeNextIdStr());
                        if (iLoginService.save(login)) {
                            StpUtil.login(login.getLoginId());
                            loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                        }
                    } else {
                        throw new IllegalMobileException();
                    }
                }
            } else {
                throw new IllegalSmsCodeException();
            }
        }
        return loginResultVo;
    }

    /**
     * @description: 构造AuthRequest实例
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:43
     **/
    private AuthRequest getAuthRequest(String client) {
        LoginOAuthSourceEnum oAuthType;

        try {
            oAuthType = LoginOAuthSourceEnum.valueOf(client.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new IllegalOAuthTypeException();
        }
        OAuthConfig config = oAuthList.getClients().get(client.toLowerCase());

        AuthConfig build = AuthConfig.builder().clientId(config.getClientId()).clientSecret(config.getClientSecret()).redirectUri(config.getCallbackUri()).build();

        return switch (oAuthType) {
            case GITHUB -> new AuthGithubRequest(build);
            case MI -> new AuthMiRequest(build);
        };
    }
}
