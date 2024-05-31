package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.vo.form.SpuCategoryForm;
import com.mi.aftersales.vo.form.SpuCategoryVisibleSetForm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
;
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
class SpuCategoryControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(SpuCategoryControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private SpuCategoryController spuCategoryController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc( MockMvcBuilders.standaloneSetup(spuCategoryController).build());
    }

    @Test
    void postSpuCategory() throws Exception {
        SpuCategoryForm form = new SpuCategoryForm();
        form.setVisible(false);
        form.setWeight(15);
        form.setCategoryName("小米15");
        form.setParentCategoryId(7);
        form.setCategoryLevel(0);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/spuCategory/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void listSpuCategory4Client() throws Exception {
        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/spuCategory/list/client");

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void setVisible() throws Exception {
        SpuCategoryVisibleSetForm form = new SpuCategoryVisibleSetForm();
        form.setVisible(false);
        form.setCategoryId(8);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/spuCategory/visible", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }
}