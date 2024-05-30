package com.mi.aftersales.controller;

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

import static org.junit.jupiter.api.Assertions.*;
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
    private OrderStatusLogController orderStatusLogController;

    private MockMvc mockMvc;


    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(orderStatusLogController).build();
    }

    @Test
    void listOrderStatusLogById() throws Exception {
        String orderId = "1795464726806106112";

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.get("/aftersales/orderStatusLog/" + orderId)
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }
}