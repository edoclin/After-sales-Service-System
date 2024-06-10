package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;


import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.FapiaoFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateFapiaoFormVo;
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

import java.time.LocalDateTime;

/**
 * @author QYenon
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class FapiaoControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(FapiaoControllerTest.class);

    @Resource
    private FapiaoController fapiaoController;

    @Resource
    private TestConfig testConfig;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(fapiaoController).build());
    }

    @Test
    void postFapiao() throws Exception {
        FapiaoFormVo form = new FapiaoFormVo();
        form.setFapiaoCode("Test1234");
        form.setFapiaoNo("Test11123");
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/fapiao/client",strJson);

    }

    @Test
    void listFapiao() throws Exception {

        testConfig.getMockMvcResult("/aftersales/fapiao/client");

    }

    @Test
    void listFapiaoByLoginId() throws Exception {
        String loginId = "";

        testConfig.getMockMvcResult("/aftersales/fapiao/manager" + loginId);

    }

    @Test
    void delFapiao() throws Exception {

        String fapiaoId = "1796051108574253056";

        testConfig.deleteMockMvcResult("/aftersales/fapiao/client/" + fapiaoId);

    }

    @Test
    void updateFapiao() throws Exception {
        UpdateFapiaoFormVo form = new UpdateFapiaoFormVo();
        form.setFapiaoId("1796051108574253056");
        form.setFapiaoInfo("111222");
        form.setFapiaoTime(LocalDateTime.of(2024, 5, 30, 13, 28));
        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/fapiao/client",strJson);

    }
}