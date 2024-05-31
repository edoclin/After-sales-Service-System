package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;

import com.mi.aftersales.vo.form.ManngerUpdateMaterialForm;
import com.mi.aftersales.vo.form.MaterialForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;


import javax.annotation.Resource;

import java.math.BigDecimal;



/**
 * @author QYenon
 * @create 2024/5/29
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration

class MaterialControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(MaterialControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private MaterialController materialController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(materialController).build());
    }

    @Test
    public void addMaterial() throws Exception {
        MaterialForm form = new MaterialForm();
        form.setMaterialName("Test11223");
        form.setMaterialDesc("TestAddMaterial");
        form.setUnit("test");
        form.setCost(BigDecimal.valueOf(5));
        form.setPrice(BigDecimal.valueOf(10));
        form.setStock(BigDecimal.valueOf(20));
        form.setAlertNum(BigDecimal.valueOf(2));

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/material/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void updateMaterial() throws Exception {

        // 创建一个更新物料表单对象
        ManngerUpdateMaterialForm form = new ManngerUpdateMaterialForm();
        form.setMaterialId("1795767106466471936");
        form.setMaterialName("UpdatedTest1");
        form.setMaterialDesc("UpdatedTestUpdateMaterial");
        form.setUnit("test");
        form.setCost(BigDecimal.valueOf(5));
        form.setPrice(BigDecimal.valueOf(10));
        form.setStock(BigDecimal.valueOf(20));
        form.setAlertNum(BigDecimal.valueOf(2));

        String strJson = JSON.toJSONString(form);

        // 发送PUT请求，更新物料信息
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/material/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }
}