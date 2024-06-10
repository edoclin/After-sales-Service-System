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
class OrderStatusLogControllerTest {
    private static final Logger logger = LoggerFactory.getLogger(OrderStatusLogControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private OrderStatusLogController orderStatusLogController;

    @BeforeEach
    void setUp() {

        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(orderStatusLogController).build());
    }

    @Test
    void listOrderStatusLogById() throws Exception {
        String orderId = "1794929431970746368";

        testConfig.getMockMvcResult("/aftersales/order-status-log/" + orderId);

    }
}