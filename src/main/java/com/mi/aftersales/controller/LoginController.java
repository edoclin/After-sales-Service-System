package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.captcha.generator.RandomGenerator;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.config.yaml.bean.SmsConfig;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.exception.graceful.IllegalOAuthTypeException;
import com.mi.aftersales.service.ILoginService;
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
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
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

//
//    @Resource
//    public void setRedisTemplate(RedisTemplate redisTemplate) {
//        RedisSerializer stringSerializer = new StringRedisSerializer();
//        redisTemplate.setKeySerializer(stringSerializer);
//        redisTemplate.setValueSerializer(stringSerializer);
//        redisTemplate.setHashKeySerializer(stringSerializer);
//        redisTemplate.setHashValueSerializer(stringSerializer);
//        this.redisTemplate = redisTemplate;
//    }

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
    private SmsConfig smsConfig;

    @GetMapping(path = "/sms")
    @Parameter(name = "mobile", description = "发送验证码", example = "13111111111", required = true)
    public SmsResultVo sendSmsCode(@RequestBody @Valid SendSmsCodeForm form) {

        SmsResultVo smsResultVo = new SmsResultVo();
        RandomGenerator randomGenerator = new RandomGenerator("0123456789", 6);

        String code = randomGenerator.generate() + "_" + DateUtil.currentSeconds();

        if (StrUtil.isBlank(redisTemplate4Sms.opsForValue().get(form.getMobile()))) {
            // 不存在, 发送验证码

            // todo send sms
            redisTemplate4Sms.opsForValue().set(form.getMobile(), code, smsConfig.getValidTime(), TimeUnit.SECONDS);
            smsResultVo.setSuccess(true);
        } else {
            // 存在, 判断发送时间
            long timestamp = Long.parseLong(redisTemplate4Sms.opsForValue().get(form.getMobile()).split("_")[1]);
            Long delta = DateUtil.currentSeconds() - timestamp;
            if (delta <= smsConfig.getPeriod()) {
                // 间隔时间小于周期
                smsResultVo.setSuccess(false);
                smsResultVo.setInfo(CharSequenceUtil.format("请{}秒后再尝试!", smsConfig.getPeriod() - delta));
            } else {
                // todo send sms
                redisTemplate4Sms.opsForValue().set(form.getMobile(), code, smsConfig.getValidTime(), TimeUnit.SECONDS);
                smsResultVo.setSuccess(true);
            }
        }
        return smsResultVo;
    }

    /**
     * @description: 三方登录页面渲染
     * @return: 三方登录url
     * @author: edoclin
     * @created: 2024/5/12 12:36
     **/
    @GetMapping(path = "/oauth2/{client}/render")
    @Operation(summary = "三方登录页面渲染", description = "三方登录页面渲染")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)

    public ThirdLoginPageResultVo githubRender(@PathVariable String client) {
        return new ThirdLoginPageResultVo().setUrl(getAuthRequest(client).authorize(AuthStateUtils.createState()));
    }

    /**
     * @description: 三方登录回调接口
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:43
     **/
    @GetMapping("/oauth2/{client}/callback")
    @Operation(summary = "三方登录回调接口", description = "三方登录回调接口")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)
    @Parameter(name = "callback", description = "回调参数", example = "github", required = true)
    public LoginResultVo login(@PathVariable String client, AuthCallback callback) {
        AuthResponse authResponse = getAuthRequest(client).login(callback);
        if (authResponse.getCode() == OAUTH2_SUCCESS_CODE) {
            // 三方授权成功
            AuthUser data = (AuthUser) authResponse.getData();
            Login login = iLoginService.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getSource, client.toUpperCase()).eq(Login::getAppId, data.getUuid()));

            if (BeanUtil.isEmpty(login)) {
                // 尚未注册, 让用户绑定手机
            } else {
                StpUtil.login(login.getLoginId());
                LoginResultVo loginResultVo = new LoginResultVo();
                loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue());
                return loginResultVo;
            }
        }
        return null;
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
        OAuthConfig config = oAuthList.getClients().get(client);

        AuthConfig build = AuthConfig.builder().clientId(config.getClientId()).clientSecret(config.getClientSecret()).redirectUri(config.getCallbackUri()).build();

        return switch (oAuthType) {
            case GITHUB -> new AuthGithubRequest(build);
            case MI -> new AuthMiRequest(build);
        };
    }


}
