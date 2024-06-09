package com.mi.aftersales;


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
    }

}
