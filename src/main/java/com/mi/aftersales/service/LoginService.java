package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.LoginBindForm;
import com.mi.aftersales.vo.form.LoginBySmsForm;
import com.mi.aftersales.vo.form.SendSmsCodeForm;
import com.mi.aftersales.vo.result.LoginResultVo;
import com.mi.aftersales.vo.result.SmsResultVo;
import com.mi.aftersales.vo.result.ThirdLoginPageResultVo;
import me.zhyd.oauth.model.AuthCallback;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/29 15:07
 **/
public interface LoginService {
    void checkLogin();

    void logout();

    SmsResultVo sendSmsCode(SendSmsCodeForm form);

    ThirdLoginPageResultVo githubRender(String client);

    LoginResultVo callback(String client, AuthCallback callback);

    LoginResultVo bind(LoginBindForm form);

    LoginResultVo loginBySms(LoginBySmsForm form);
}
