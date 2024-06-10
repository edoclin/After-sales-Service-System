package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.pojo.vo.form.LoginRoleFormVo;
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
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class LoginRoleControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(LoginRoleControllerTest.class);

    @Resource
    private LoginRoleController loginRoleController;

    @Resource
    private TestConfig testConfig;

    private MockMvc mockMvc;


    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(loginRoleController).build());
    }

    @Test
    void postLoginRole() throws Exception {
        EmployeeRoleEnum[] roles = {
                EmployeeRoleEnum.ENGINEER,
                EmployeeRoleEnum.MATERIAL_MANAGER,
                EmployeeRoleEnum.SYSTEM_MANAGER
        };

        LoginRoleFormVo form = new LoginRoleFormVo();
        form.setLoginId("1794929431970746368");
        form.setRoles(roles);

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/login-role/",strJson);

    }
}