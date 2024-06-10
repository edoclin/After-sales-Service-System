package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.LoginPermissionFormVo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import javax.annotation.Resource;

/**
 * @author QYenon
 * @created:  2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class LoginPermissionControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(LoginPermissionControllerTest.class);

    @Resource
    private LoginPermissionController loginPermissionController;

    @Resource
    private TestConfig testConfig;


    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(loginPermissionController).build());
    }

    @Test
    void postMiddleLoginPermission() throws Exception {
        LoginPermissionFormVo form = new LoginPermissionFormVo();
        form.setLoginId("1794929431970746368");
        form.setPermissionId("1");


        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/login-permission/manager",strJson);

    }
}