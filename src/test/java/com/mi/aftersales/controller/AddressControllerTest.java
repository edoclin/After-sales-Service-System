package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.alibaba.fastjson.JSON;
import com.mi.aftersales.aspect.CheckLoginAspect;
import com.mi.aftersales.entity.Address;
import com.mi.aftersales.vo.form.ClientAddressForm;
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

import static org.mockito.Mockito.*;

/**
 * @author QYenon
 * @create 2024/5/29
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class AddressControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(AddressControllerTest.class);


    @Resource
    private AddressController addressController;



    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(addressController).build();
    }

    @Test
    public void postAddress() throws Exception {

        // 创建客户地址表单
        ClientAddressForm form = new ClientAddressForm();
        form.setReceiver("postAddressTest1234");
        form.setDefaulted(false);
        form.setMobile("18111111111");
        form.setRegion("test");
        form.setAddressDetail("test");
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.post("/aftersales/address/")
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());


    }

    @Test
    public void listAddress() throws Exception {
        MvcResult mvcResult =  mockMvc.perform(
                MockMvcRequestBuilders.get("/aftersales/address/")
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void defaultAddress() throws Exception {

        String addressId = "1795830688256237568";

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.put("/aftersales/address/defaulted/" + addressId)
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
        )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void deleteAddress() throws Exception {

        String addressId = "1795830688256237568";

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.delete("/aftersales/address/" + addressId)
                        .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
        )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());
    }
}