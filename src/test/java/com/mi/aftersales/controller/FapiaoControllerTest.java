package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;


import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;

import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

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
    private TestConfig testConfig;

    @Resource
    private FapiaoController fapiaoController;

    private MockMvc mockMvc;


    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(fapiaoController).build());
    }

    @Test
    void postFapiao() throws Exception {
        FapiaoForm form = new FapiaoForm();
        form.setFapiaoCode("Test1234");
        form.setFapiaoNo("Test11123");
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/fapiao/", strJson);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());
    }

    @Test
    void listFapiao() throws Exception {

        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/fapiao/client");

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());


    }

    @Test
    void delFapiao() throws Exception {

        String fapiaoId = "1796405881106317312";

        MvcResult mvcResult = testConfig.deleteMockMvcResult("/aftersales/fapiao/client/" + fapiaoId);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    void updateFapiao() throws Exception {
        UpdateFapiaoForm form = new UpdateFapiaoForm();
        form.setFapiaoId("1796405881106317312");
        form.setFapiaoInfo("111222");
        form.setFapiaoTime(LocalDateTime.of(2024, 5, 30, 13, 28));
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/fapiao/client", strJson);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }
}