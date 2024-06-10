package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.ClientAddressFormVo;
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
 * @create 2024/5/29
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class AddressControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(AddressControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private AddressController addressController;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(addressController).build());
    }

    @Test
    public void postAddress() throws Exception {

        // 创建客户地址表单
        ClientAddressFormVo form = new ClientAddressFormVo();
        form.setReceiver("postAddressTest111");
        form.setDefaulted(false);
        form.setMobile("18111111111");
        form.setRegion("test");
        form.setAddressDetail("test");
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/address/client", strJson);

    }

    @Test
    public void listAddress() throws Exception {
        testConfig.putMockMvcResult("/aftersales/address/client");
    }

    @Test
    public void defaultAddress() throws Exception {

        String addressId = "1795830688256237568";

        testConfig.putMockMvcResult("/aftersales/address/client/defaulted/" + addressId);

    }

    @Test
    public void deleteAddress() throws Exception {

        String addressId = "1795830688256237568";

        testConfig.deleteMockMvcResult("/aftersales/address/client/" + addressId);
    }
}