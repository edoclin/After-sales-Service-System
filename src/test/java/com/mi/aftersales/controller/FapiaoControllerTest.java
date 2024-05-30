package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;


import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
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

import java.time.LocalDate;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.*;

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



    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(fapiaoController).build();
    }

    @Test
    void postFapiao() throws Exception {
        FapiaoForm form = new FapiaoForm();
        form.setFapiaoCode("Test1234");
        form.setFapiaoNo("Test11123");
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.post("/aftersales/fapiao/")
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                        .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(strJson)
        )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());
    }

    @Test
    void listFapiao() throws Exception {

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.get("/aftersales/fapiao/client")
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());


    }

    @Test
    void delFapiao() throws Exception {

        String fapiaoId = "1796051108574253056";

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.delete("/aftersales/fapiao/client/" + fapiaoId)
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    void updateFapiao() throws Exception {
        UpdateFapiaoForm form = new UpdateFapiaoForm();
        form.setFapiaoId("1796051108574253056");
        form.setFapiaoInfo("111222");
        form.setFapiaoTime(LocalDateTime.of(2024, 5, 30, 13, 28));
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.put("/aftersales/fapiao/client" )
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }
}