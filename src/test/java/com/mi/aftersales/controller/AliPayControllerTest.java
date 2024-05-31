package com.mi.aftersales.controller;

import com.mi.aftersales.config.TestConfig;
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

class AliPayControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(AliPayControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private AliPayController aliPayController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(aliPayController).build());
    }

    @Test
    public void alipay() throws Exception {

        String orderId = "1795465445231661056";

        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/alipay/pay/" + orderId);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void returnCallback() throws Exception {

        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/alipay/return", "outTradeNo", "" );

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }
}