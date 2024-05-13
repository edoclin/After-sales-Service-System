package com.mi.aftersales.controller;

import com.mi.aftersales.config.yaml.bean.OAuthConfig;
import com.mi.aftersales.config.yaml.bean.OAuthList;
import com.mi.aftersales.enums.OAuthType;
import me.zhyd.oauth.config.AuthConfig;
import me.zhyd.oauth.model.AuthCallback;
import me.zhyd.oauth.request.AuthGithubRequest;
import me.zhyd.oauth.request.AuthMiRequest;
import me.zhyd.oauth.request.AuthRequest;
import me.zhyd.oauth.utils.AuthStateUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @description: 基于AuthRequest的三方登录restful API
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:35
 **/
@RestController
@RequestMapping(path = "/oauth")
public class OAuthController {
    @Resource
    private OAuthList oAuthList;

    /**
     * @description: 三方登录页面渲染
     * @return: 三方登录url
     * @author: edoclin
     * @created: 2024/5/12 12:36
     **/
    @GetMapping(path = "/{client}/render")
    public String githubRender(@PathVariable String client) {
        return getAuthRequest(client).authorize(AuthStateUtils.createState());
    }

    /**
     * @description: 三方登录回调接口
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:43
     **/
    @GetMapping("/{client}/callback")
    public Object login(@PathVariable String client, AuthCallback callback) {
        return getAuthRequest(client).login(callback);
    }

    /**
     * @description: 构造AuthRequest实例
     * @return:
     * @author: edoclin
     * @created: 2024/5/12 12:43
     **/
    private AuthRequest getAuthRequest(String client) {
        OAuthConfig config = oAuthList.getClients().get(client);
        AuthConfig build = AuthConfig.builder().clientId(config.getClientId()).clientSecret(config.getClientSecret()).redirectUri(config.getCallbackUri()).build();
        OAuthType oAuthType = OAuthType.valueOf(client);

        return switch (oAuthType) {
            case GITHUB -> new AuthGithubRequest(build);
            case MI -> new AuthMiRequest(build);
        };
    }
}
