package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.UpdateSkuFormVo;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
import com.mi.aftersales.pojo.vo.form.SkuFormVo;
import com.mi.aftersales.pojo.vo.form.SkuVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSkuVisibleFormVo;
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
    private SkuController skuController;

    private MockMvc mockMvc;

    @Resource
    private TestConfig testConfig;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(skuController).build());
    }

    @Test
    void postSpu() throws Exception {
        SkuFormVo form = new SkuFormVo();
        form.setSpuId("1791442641188290560");
        form.setSkuDisplayName("小米14Pro(24GB+2048GB)");
        form.setSkuCoverFileId("1796067339024977920");
        form.setVisible(false);
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/sku/manager",strJson);

    }

    @Test
    void updateSpu() throws Exception {

        UpdateSkuFormVo form = new UpdateSkuFormVo();
        form.setSkuId("1796084357518204928");
        form.setSpuId("1791442641188290560");
        form.setSkuCoverFileId("1800068261165916160");
        form.setSkuDisplayName("小米14Pro(24GB+2048GB)");

        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/sku/manager",strJson);

    }

    @Test
    void putSpuVisible() throws Exception {
        UpdateSkuVisibleFormVo form = new UpdateSkuVisibleFormVo();
        form.setSkuId("1796084357518204928");
        form.setVisible(true);

        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/sku/manager/visible",strJson);

    }

    @Test
    void list4Client() throws Exception {
        String spuId = "1791442606358790144";
        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/sku/client/" + spuId, strJson);

    }

    @Test
    void conditionList() throws Exception {
        List<QueryParam> list = new ArrayList<>();


        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);
        form.setParams(list);

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/sku/manager/query", strJson);

    }


    @Test
    void deleteSkuById() throws Exception {

        String skuId = "1796084357518204928";

        testConfig.deleteMockMvcResult("/aftersales/sku/manager" + skuId);

    }
}