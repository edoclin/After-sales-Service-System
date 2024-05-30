package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.service.IClientServiceCenterService;
import com.mi.aftersales.vo.form.ClientServiceCenterForm;
import com.mi.aftersales.vo.form.UpdateClientServiceCenterForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockReset;
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
class ClientServiceCenterControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(ClientServiceCenterControllerTest.class);

    @Resource
    private ClientServiceCenterController clientServiceCenterController;


    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(clientServiceCenterController).build();
    }

    @Test
    public void postClientServiceCenter() throws Exception {
        ClientServiceCenterForm form = new ClientServiceCenterForm();
        form.setRegion("TestClientServiceCenter");
        form.setAddressDetail("Test");
        form.setMobile("13111111111");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.post("/aftersales/clientServiceCenter/")
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
    public void putClientServiceCenter() throws Exception {
        UpdateClientServiceCenterForm form = new UpdateClientServiceCenterForm();
        form.setCenterId("1796055029258641408");
        form.setRegion("TestClientServiceCenter.......");
        form.setAddressDetail("Test...");
        form.setMobile("1311111111");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.put("/aftersales/clientServiceCenter/")
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