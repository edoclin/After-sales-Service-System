package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.vo.form.ClientAddressForm;
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

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(addressController).build());
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

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/address/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void listAddress() throws Exception {
        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/address/");

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void defaultAddress() throws Exception {

        String addressId = "1796103581427015680";

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/address/defaulted/" + addressId);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void deleteAddress() throws Exception {

        String addressId = "1796103581427015680";

        MvcResult mvcResult = testConfig.deleteMockMvcResult("/aftersales/address/" + addressId);

        logger.info("调用返回的结果：{}",mvcResult.getResponse().getContentAsString());
    }
}