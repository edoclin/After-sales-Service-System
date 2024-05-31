package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import com.mi.aftersales.vo.form.LoginRoleForm;
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
class LoginRoleControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(LoginRoleControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private LoginRoleController loginRoleController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc( MockMvcBuilders.standaloneSetup(loginRoleController).build());
    }

    @Test
    void postLoginRole() throws Exception {
        EmployeeRoleEnum[] roles = {
                EmployeeRoleEnum.ENGINEER,
                EmployeeRoleEnum.MATERIAL_MANAGER,
                EmployeeRoleEnum.SYSTEM_MANAGER
        };

        LoginRoleForm form = new LoginRoleForm();
        form.setLoginId("1794929431970746368");
        form.setRoles(roles);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/loginRole/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());



    }
}