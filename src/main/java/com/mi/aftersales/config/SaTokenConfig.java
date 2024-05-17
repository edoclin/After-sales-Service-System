package com.mi.aftersales.config;

import cn.dev33.satoken.session.SaSession;
import cn.dev33.satoken.stp.StpInterface;
import cn.dev33.satoken.stp.StpUtil;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

/**
 * @description: SaToken框架配置
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 22:16
 **/
@Configuration
public class SaTokenConfig implements WebMvcConfigurer, StpInterface {
    /**
     * 返回指定账号id所拥有的权限码集合
     * Params:
     * loginId – 账号id
     * loginType – 账号类型
     * Returns:
     * 该账号id具有的权限码集合
     */
    @Override
    public List<String> getPermissionList(Object o, String s) {
        return (List<String>) StpUtil.getSession().get(SaSession.PERMISSION_LIST);

    }

    /**
     * 返回指定账号id所拥有的角色标识集合
     * Params:
     * loginId – 账号id
     * loginType – 账号类型
     * Returns:
     * 该账号id具有的角色标识集合
     */
    @Override
    public List<String> getRoleList(Object o, String s) {
        return (List<String>) StpUtil.getSession().get(SaSession.ROLE_LIST);
    }
}
