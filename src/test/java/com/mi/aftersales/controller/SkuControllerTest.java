package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
import com.mi.aftersales.vo.form.SkuForm;
import com.mi.aftersales.vo.form.SkuVisibleSetForm;
import com.mi.aftersales.vo.form.UpdateSkuVisibleForm;
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

import java.util.ArrayList;
import java.util.List;


/**
 * @author QYenon
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class SkuControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(SkuControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private SkuController skuController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(skuController).build());
    }

    @Test
    void postSpu() throws Exception {
        SkuForm form = new SkuForm();
        form.setSpuId("1791442641188290560");
        form.setSkuDisplayName("小米14Pro(16GB+512GB)");
        form.setSkuCoverFileId("1796067339024977920");
        form.setVisible(false);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/sku/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void putSpuVisible() throws Exception {
        UpdateSkuVisibleForm form = new UpdateSkuVisibleForm();
        form.setSkuId("1796084357518204928");
        form.setVisible(true);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/sku/visible", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void list4Client() throws Exception {
        String spuId = "1791442606358790144";
        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/sku/client/" + spuId, strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void list() throws Exception {
        String spuId = "1791442606358790144";
        List<QueryParam> list = new ArrayList<>();


        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);
        form.setParams(list);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/sku/list/" + spuId, strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }


    @Test
    void testList() throws Exception {
        SkuVisibleSetForm form = new SkuVisibleSetForm();
        form.setSkuId("1796084357518204928");
        form.setVisible(true);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/sku/visible/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }
}