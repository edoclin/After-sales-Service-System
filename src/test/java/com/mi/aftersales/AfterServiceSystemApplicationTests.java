package com.mi.aftersales;


import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.service.ILoginService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import javax.annotation.Resource;

@SpringBootTest(classes = AftersalesServiceSystem.class)
class AfterServiceSystemApplicationTests {

    @Resource
    private ILoginService iLoginService;
    @Test
    void test() {

        Login login = new Login()
                .setMobile("123")
                .setSource(LoginOAuthSourceEnum.MI)
                .setLoginType(LoginTypeEnum.CLIENT);
        iLoginService.save(login);
    }

}
