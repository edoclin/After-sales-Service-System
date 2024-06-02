package com.mi.aftersales.service;

import com.mi.aftersales.pojo.vo.form.LoginBindFormVo;
import com.mi.aftersales.pojo.vo.form.LoginBySmsFormVo;
import com.mi.aftersales.pojo.vo.form.SendSmsCodeFormVo;
import com.mi.aftersales.pojo.vo.LoginResultVo;
import com.mi.aftersales.pojo.vo.SmsResultVo;
import com.mi.aftersales.pojo.vo.ThirdLoginPageResultVo;
import me.zhyd.oauth.model.AuthCallback;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/29 15:07
 **/
public interface LoginService {
    void checkLogin();

    void logout();

    SmsResultVo sendSmsCode(SendSmsCodeFormVo form);

    ThirdLoginPageResultVo githubRender(String client);

    LoginResultVo callback(String client, AuthCallback callback);

    LoginResultVo bind(LoginBindFormVo form);

    LoginResultVo loginBySms(LoginBySmsFormVo form);
}
