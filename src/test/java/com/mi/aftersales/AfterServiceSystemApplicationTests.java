package com.mi.aftersales;


import cn.hutool.core.lang.Assert;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.enums.entity.LoginOAuthSourceEnum;
import com.mi.aftersales.enums.entity.LoginTypeEnum;
import com.mi.aftersales.exception.graceful.BaseCustomException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.repository.ILoginRepository;
import org.apache.http.util.Asserts;
import org.assertj.core.internal.bytebuddy.description.type.TypeDefinition;
import org.assertj.core.internal.bytebuddy.matcher.ElementMatcher;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.core.AnyOf;
import org.junit.jupiter.api.Test;
import org.mockito.junit.MockitoJUnit;
import org.springframework.boot.test.context.SpringBootTest;

import javax.annotation.Resource;

import static org.assertj.core.internal.bytebuddy.matcher.ElementMatchers.anyOf;
import static org.hamcrest.MatcherAssert.assertThat;

@SpringBootTest(classes = AftersalesServiceSystem.class)
class AfterServiceSystemApplicationTests {

    @Resource
    private ILoginRepository iLoginRepository;
    @Test
    void test() {
    }

}
