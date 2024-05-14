package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.exception.graceful.IllegalOAuthTypeException;
import com.mi.aftersales.service.ILoginService;
import com.mi.aftersales.vo.LoginPageVo;
import com.mi.aftersales.vo.LoginVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import me.zhyd.oauth.config.AuthConfig;
import me.zhyd.oauth.model.AuthCallback;
import me.zhyd.oauth.model.AuthResponse;
import me.zhyd.oauth.model.AuthUser;
import me.zhyd.oauth.request.AuthGithubRequest;
import me.zhyd.oauth.request.AuthMiRequest;
import me.zhyd.oauth.request.AuthRequest;
import me.zhyd.oauth.utils.AuthStateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

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

    /**
     * @description: 三方登录页面渲染
     * @return: 三方登录url
     * @author: edoclin
     * @created: 2024/5/12 12:36
     **/
    @GetMapping(path = "/oauth2/{client}/render")
    @Operation(summary = "三方登录页面渲染", description = "三方登录页面渲染")
    @Parameter(name = "client", description = "三方登录类型", example = "github", required = true)

    public LoginPageVo githubRender(@PathVariable String client) {
        return new LoginPageVo().setUrl(getAuthRequest(client).authorize(AuthStateUtils.createState()));
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
    public LoginVo login(@PathVariable String client, AuthCallback callback) {
        AuthResponse authResponse = getAuthRequest(client).login(callback);
        if (authResponse.getCode() == OAUTH2_SUCCESS_CODE) {
            // 三方授权成功
            AuthUser data = (AuthUser) authResponse.getData();
            Login login = iLoginService.getOne(Wrappers.lambdaQuery(Login.class).eq(Login::getSource, client.toUpperCase()).eq(Login::getAppId, data.getUuid()));

            if (BeanUtil.isEmpty(login)) {
                // 尚未注册, 让用户绑定手机
            } else {
                StpUtil.login(login.getLoginId());
                LoginVo loginVo = new LoginVo();
                loginVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue());
                return loginVo;
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
