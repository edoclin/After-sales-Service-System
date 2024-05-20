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
import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.config.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.controller.enums.SmsCodeType;
import com.mi.aftersales.service.*;
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


    @Resource
    private IApiService iApiService;

    @Resource
    private ILoginService iLoginService;

    @Resource
    private IMiddlePermissionApiService iMiddlePermissionApiService;

    @Resource
    private IMiddleLoginPermissionService iMiddleLoginPermissionService;

    @Resource
    private StringRedisTemplate redisTemplate4Sms;

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

    @Resource
    private IEmployeeInfoService iEmployeeInfoService;

    @Resource
    private ILoginRoleService iLoginRoleService;

    @PostMapping(path = "/client/sms")
    @Operation(summary = "发送短信验证码", description = "发送短信验证码")
    public SmsResultVo sendSmsCode(@RequestBody @Valid SendSmsCodeForm form) {
        SmsResultVo smsResultVo = new SmsResultVo();
        RandomGenerator randomGenerator = new RandomGenerator("0123456789", 6);

        String key = "sms:" + form.getMobile();
        String smsCode = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(smsCode)) {
            String code = randomGenerator.generate();

            SmsBlend smsBlend = SmsFactory.getSmsBlend(SmsCodeType.LOGIN.getValue());
            SmsResponse smsResponse = smsBlend.sendMessage(form.getMobile(), code);

            code += "_" + DateUtil.currentSeconds();
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
                String code = randomGenerator.generate();
                SmsBlend smsBlend = SmsFactory.getSmsBlend(SmsCodeType.LOGIN.getValue());
                SmsResponse smsResponse = smsBlend.sendMessage(form.getMobile(), code);

                code += "_" + DateUtil.currentSeconds();
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


    @GetMapping(path = "/oauth2/{client}/callback")
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
                login(login);
                loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
            }
        } else {
            log.error(authResponse.getMsg());
            throw new GracefulResponseException(CharSequenceUtil.format("{}登录失败", client));
        }
        return loginResultVo;
    }

    @PostMapping(path = "/bind")
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
            throw new GracefulResponseException("验证码无效");
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
                    throw new GracefulResponseException("无效的临时令牌");
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
                    login(login);
                    loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                }

            } else {
                throw new GracefulResponseException("验证码错误");
            }
        }
        return loginResultVo;
    }


    @PostMapping(path = "/sms")
    @Operation(summary = "手机验证码登录", description = "手机验证码登录")
    @Parameter(name = "mobile", description = "手机号", example = "13111111111", required = true)
    @Parameter(name = "code", description = "短信验证码", example = "123456", required = true)
    @Parameter(name = "autoRegister", description = "未注册用户是否自动注册", example = "true", required = true)
    public LoginResultVo loginBySms(@RequestBody @Valid LoginBySmsForm form) {
        LoginResultVo loginResultVo = new LoginResultVo();
        // 验证验证码是否正确
        String key = "sms:" + form.getMobile();
        String code = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(code)) {
            throw new GracefulResponseException("验证码已过期");
        } else {
            code = code.split("_")[0];
            if (CharSequenceUtil.equals(form.getCode(), code)) {
                redisTemplate4Sms.delete(key);
                // 验证码正确
                Login login = iLoginService.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getMobile, form.getMobile()));
                if (BeanUtil.isNotEmpty(login)) {
                    login(login);
                    loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                } else {
                    if (Boolean.TRUE.equals(form.getAutoRegister())) {
                        // 自动注册
                        login = new Login();
                        login.setMobile(form.getMobile());
                        login.setLoginType(LoginTypeEnum.CLIENT);
                        login.setLoginId(IdUtil.getSnowflakeNextIdStr());
                        if (iLoginService.save(login)) {
                            login(login);
                            loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                        }
                    } else {
                        throw new GracefulResponseException("当前手机号未注册");
                    }
                }
            } else {
                throw new GracefulResponseException("验证码错误");
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
            throw new GracefulResponseException("不支持的登录方式");
        }
        OAuthConfig config = oAuthList.getClients().get(client.toLowerCase());

        AuthConfig build = AuthConfig.builder().clientId(config.getClientId()).clientSecret(config.getClientSecret()).redirectUri(config.getCallbackUri()).build();

        return switch (oAuthType) {
            case GITHUB -> new AuthGithubRequest(build);
            case MI -> new AuthMiRequest(build);
        };
    }

    /**
     * @description: 登录时缓存权限信息，权限修改后需要重新登录
     * @return:
     * @author: edoclin
     * @created: 2024/5/16 23:29
     **/
    private void login(Login login) {
        StpUtil.login(login.getLoginId());
        List<String> permissions = new ArrayList<>();
        iMiddleLoginPermissionService.lambdaQuery().eq(MiddleLoginPermission::getLoginId, login.getLoginId()).list().forEach(middleLoginPermission -> iMiddlePermissionApiService.lambdaQuery().eq(MiddlePermissionApi::getPermissionId, middleLoginPermission.getPermissionId()).list().forEach(middlePermissionApi -> {
            Api api = iApiService.getById(middlePermissionApi.getApiId());

            if (BeanUtil.isNotEmpty(api)) {
                permissions.add(CharSequenceUtil.format("{}-{}", api.getMethod().name().toUpperCase(), api.getUri()));
            }
        }));
        StpUtil.getSession().set(SaSession.PERMISSION_LIST, permissions);

        ArrayList<String> roles = new ArrayList<>();

        // 默认具有CLIENT角色
        roles.add(LoginTypeEnum.CLIENT.name());
        iLoginRoleService.lambdaQuery().eq(LoginRole::getLoginId, login.getLoginId()).list().forEach(loginRole -> {
            roles.add(loginRole.getEmployeeRole().name());
        });

        StpUtil.getSession().set(SaSession.ROLE_LIST, roles);

    }
}
