package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;

import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.ClientServiceCenterFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateClientServiceCenterFormVo;
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
class ClientServiceCenterControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(ClientServiceCenterControllerTest.class);

    @Resource
    private ClientServiceCenterController clientServiceCenterController;

    @Resource
    private TestConfig testConfig;


    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(clientServiceCenterController).build());
    }

    @Test
    public void postClientServiceCenter() throws Exception {
        ClientServiceCenterFormVo form = new ClientServiceCenterFormVo();
        form.setRegion("TestClientServiceCenter");
        form.setAddressDetail("Test");
        form.setMobile("13111111111");

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/client-service-center/manager",strJson);
    }

    @Test
    public void putClientServiceCenter() throws Exception {
        UpdateClientServiceCenterFormVo form = new UpdateClientServiceCenterFormVo();
        form.setCenterId("1796055029258641408");
        form.setRegion("TestClientServiceCenter.......");
        form.setAddressDetail("Test...");
        form.setMobile("1311111111");

        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/client-service-center/manager",strJson);
    }
}