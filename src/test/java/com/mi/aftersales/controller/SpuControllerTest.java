package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.SpuFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuVisibleFormVo;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
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
class SpuControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(SkuControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private SpuController spuController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(spuController).build());
    }

    @Test
    void postSpu() throws Exception {
        SpuFormVo form = new SpuFormVo();
        form.setCategoryId(11);
        form.setWeight(Short.valueOf("1"));
        form.setSpuName("小米12");
        form.setSpuCoverFileId("1796067337615691776");
        form.setSpuDesc("小米12系列手机");
        form.setVisible(false);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/spu/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void putSpuVisible() throws Exception {
        UpdateSpuVisibleFormVo form = new UpdateSpuVisibleFormVo();
        form.setSpuId("1796096627564597248");
        form.setVisible(true);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/spu/visible", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void list4Client() throws Exception {
        Integer categoryId = 10;
        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/spu/client/" + categoryId, strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void list() throws Exception {

        Integer categoryId = 10;
        List<QueryParam> list = new ArrayList<>();

        ConditionQuery form = new ConditionQuery();
        form.setCurrent(1L);
        form.setLimit(10L);
        form.setParams(list);

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/spu/list/" + categoryId, strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }
}