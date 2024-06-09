package com.mi.aftersales.service.impl;

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
import com.mi.aftersales.common.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.common.yaml.bean.OAuthConfig;
import com.mi.aftersales.common.yaml.bean.OAuthList;
import com.mi.aftersales.enums.controller.SmsType;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.enums.entity.LoginOAuthSourceEnum;
import com.mi.aftersales.enums.entity.LoginTypeEnum;
import com.mi.aftersales.repository.*;
import com.mi.aftersales.service.AsyncTaskService;
import com.mi.aftersales.service.LoginService;
import com.mi.aftersales.pojo.vo.form.LoginBindFormVo;
import com.mi.aftersales.pojo.vo.form.LoginBySmsFormVo;
import com.mi.aftersales.pojo.vo.form.SendSmsCodeFormVo;
import com.mi.aftersales.pojo.vo.LoginResultVo;
import com.mi.aftersales.pojo.vo.SmsResultVo;
import com.mi.aftersales.pojo.vo.ThirdLoginPageResultVo;
import me.zhyd.oauth.config.AuthConfig;
import me.zhyd.oauth.model.AuthCallback;
import me.zhyd.oauth.model.AuthResponse;
import me.zhyd.oauth.model.AuthUser;
import me.zhyd.oauth.request.AuthGithubRequest;
import me.zhyd.oauth.request.AuthMiRequest;
import me.zhyd.oauth.request.AuthRequest;
import me.zhyd.oauth.utils.AuthStateUtils;
import org.dromara.sms4j.api.SmsBlend;
import org.dromara.sms4j.api.callback.CallBack;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.dromara.sms4j.core.factory.SmsFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.common.RedisNamespace.SMS_PREFIX;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/29 12:23
 **/
@Service
public class LoginServiceImpl implements LoginService {

    private static final Logger log = LoggerFactory.getLogger(LoginServiceImpl.class);

    @Resource
    private AsyncTaskService asyncTaskService;
    /**
     * @description: 三方登录成功状态码
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 16:35
     **/
    private static final int OAUTH2_SUCCESS_CODE = 2000;

    @Resource
    private ILoginRepository iLoginRepository;

    @Resource
    private StringRedisTemplate redisTemplate4Sms;

    @Resource
    private OAuthList oAuthList;

    @Resource
    private CustomSmsConfig customSmsConfig;

    @Override
    public void checkLogin() {
        // 根据返回值判断是否登录
    }

    @Override
    public void logout() {
        StpUtil.logout();
    }

    @Override
    public SmsResultVo sendSmsCode(SendSmsCodeFormVo form) {
        SmsResultVo smsResultVo = new SmsResultVo();

        String key = SMS_PREFIX + form.getMobile();
        String smsCode = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(smsCode)) {
            sendCode(form.getMobile(), key);
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
                sendCode(form.getMobile(), key);
                smsResultVo.setSuccess(true);
            }
        }
        return smsResultVo;
    }

    private void sendCode(String mobile, String cacheKey) {
        RandomGenerator randomGenerator = new RandomGenerator("0123456789", 6);

        String code = randomGenerator.generate();
        SmsBlend smsBlend = SmsFactory.getSmsBlend(SmsType.LOGIN.getValue());
        smsBlend.sendMessageAsync(mobile, code, smsResponse -> asyncTaskService.asyncSmsLog(mobile, smsResponse));

        code += "_" + DateUtil.currentSeconds();
        redisTemplate4Sms.opsForValue().set(cacheKey, code, customSmsConfig.getValidTime(), TimeUnit.SECONDS);
    }

    @Override
    public ThirdLoginPageResultVo githubRender(String client) {
        return new ThirdLoginPageResultVo().setUrl(getAuthRequest(client).authorize(AuthStateUtils.createState()));
    }

    @Override
    public LoginResultVo callback(String client, AuthCallback callback) {
        LoginResultVo loginResultVo = new LoginResultVo();
        AuthResponse authResponse = getAuthRequest(client).login(callback);
        if (authResponse.getCode() == OAUTH2_SUCCESS_CODE) {
            // 三方授权成功
            AuthUser data = (AuthUser) authResponse.getData();
            Login login = iLoginRepository.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getSource, LoginOAuthSourceEnum.valueOf(client.toUpperCase())).eq(Login::getAppId, data.getUuid()));
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

    @Override
    public LoginResultVo bind(LoginBindFormVo form) {
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
                Login login = iLoginRepository.lambdaQuery().eq(Login::getMobile, form.getMobile()).one();

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

                if (iLoginRepository.saveOrUpdate(login)) {
                    login(login);
                    loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
                }

            } else {
                throw new GracefulResponseException("验证码错误");
            }
        }
        return loginResultVo;
    }

    @Override
    public LoginResultVo loginBySms(LoginBySmsFormVo form) {
        LoginResultVo loginResultVo = new LoginResultVo();
        // 验证验证码是否正确
        String key = "sms:" + form.getMobile();
        String code = redisTemplate4Sms.opsForValue().get(key);
        if (CharSequenceUtil.isBlank(code)) {
            throw new GracefulResponseException("验证码已过期！");
        }
        code = code.split("_")[0];
        if (!CharSequenceUtil.equals(form.getCode(), code)) {
            throw new GracefulResponseException("验证码错误");
        }
        // 验证码正确
        redisTemplate4Sms.delete(key);
        Login login = iLoginRepository.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getMobile, form.getMobile()));
        if (BeanUtil.isNotEmpty(login)) {
            login(login);
            loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
        } else {
            if (Boolean.FALSE.equals(form.getAutoRegister())) {
                throw new GracefulResponseException("当前手机号未注册且未开启自动注册！");
            }

            // 自动注册
            login = new Login();
            login.setMobile(form.getMobile());
            login.setLoginType(LoginTypeEnum.CLIENT);
            login.setLoginId(IdUtil.getSnowflakeNextIdStr());
            if (iLoginRepository.save(login)) {
                login(login);
                loginResultVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue()).setLoginId(login.getLoginId());
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

        asyncTaskService.listLoginPermissions(StpUtil.getSession(), login);
    }
}
