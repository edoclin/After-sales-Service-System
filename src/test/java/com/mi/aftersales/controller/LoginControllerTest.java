package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.pojo.vo.form.LoginBySmsFormVo;
import com.mi.aftersales.pojo.vo.form.SendSmsCodeFormVo;
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
class LoginControllerTest {
    private static final Logger logger = LoggerFactory.getLogger(LoginControllerTest.class);

    @Resource
    private LoginController loginController;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(loginController).build();
    }

    @Test
    void sendSmsCode() throws Exception {
        SendSmsCodeFormVo form = new SendSmsCodeFormVo();
        form.setMobile("18845105338");
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.post("/aftersales/login/sms/send")
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
    void loginBySms()throws Exception {
        LoginBySmsFormVo form = new LoginBySmsFormVo();
        form.setMobile("18845105338");
        form.setCode("145167");
        form.setAutoRegister(true);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.post("/aftersales/login/sms")
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

}