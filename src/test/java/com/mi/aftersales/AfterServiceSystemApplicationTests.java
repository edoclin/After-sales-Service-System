package com.mi.aftersales;


import com.mi.aftersales.entity.Login;
import com.mi.aftersales.enums.entity.LoginOAuthSourceEnum;
import com.mi.aftersales.enums.entity.LoginTypeEnum;
import com.mi.aftersales.repository.ILoginRepository;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import javax.annotation.Resource;

@SpringBootTest(classes = AftersalesServiceSystem.class)
class AfterServiceSystemApplicationTests {

    @Resource
    private ILoginRepository iLoginRepository;
    @Test
    void test() {

        Login login = new Login()
                .setMobile("123")
                .setSource(LoginOAuthSourceEnum.MI)
                .setLoginType(LoginTypeEnum.CLIENT);
        iLoginRepository.save(login);
    }

}
