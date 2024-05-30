package com.mi.aftersales.controller;

import com.mi.aftersales.config.yaml.bean.AliPayConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.Extension;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
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

class AliPayControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(AliPayControllerTest.class);

    @Resource
    private AliPayController aliPayController;

    @Mock
    private AliPayConfig aliPayConfig;

//    @Mock
//    private IOrderService iOrderService;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(aliPayController).build();
    }

    @Test
    public void alipay() throws Exception {

        String orderId = "1795465445231661056";

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.get("/aftersales/alipay/pay/" + orderId)
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
        )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void returnCallback() throws Exception {

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.get("/aftersales/alipay/return")
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                        .param("outTradeNo","" )
        )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }
}