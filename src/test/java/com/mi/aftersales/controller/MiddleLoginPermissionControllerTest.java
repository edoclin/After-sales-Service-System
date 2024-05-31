package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.vo.form.LoginPermissionForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;

import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;

import org.springframework.test.web.servlet.MvcResult;

import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import javax.annotation.Resource;

/**
 * @author QYenon
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class MiddleLoginPermissionControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(MiddleLoginPermissionControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private MiddleLoginPermissionController middleLoginPermissionController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(middleLoginPermissionController).build());
    }

    @Test
    void postMiddleLoginPermission() throws Exception {
        LoginPermissionForm form = new LoginPermissionForm();
        form.setLoginId("1794929431970746368");
        form.setPermissionId("1");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/middleLoginPermission/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }
}