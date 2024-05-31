package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;

import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.vo.form.ClientServiceCenterForm;
import com.mi.aftersales.vo.form.UpdateClientServiceCenterForm;
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
class ClientServiceCenterControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(ClientServiceCenterControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private ClientServiceCenterController clientServiceCenterController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(clientServiceCenterController).build());
    }

    @Test
    public void postClientServiceCenter() throws Exception {
        ClientServiceCenterForm form = new ClientServiceCenterForm();
        form.setRegion("TestClientServiceCenter");
        form.setAddressDetail("Test");
        form.setMobile("13111111111");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/clientServiceCenter/", strJson);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void putClientServiceCenter() throws Exception {
        UpdateClientServiceCenterForm form = new UpdateClientServiceCenterForm();
        form.setCenterId("1796055029258641408");
        form.setRegion("TestClientServiceCenter.......");
        form.setAddressDetail("Test...");
        form.setMobile("1311111111");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/clientServiceCenter/", strJson);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }
}