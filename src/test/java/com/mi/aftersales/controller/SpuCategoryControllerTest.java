package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuCategoryFormVo;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
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
class SpuCategoryControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(SpuCategoryControllerTest.class);

    @Resource
    private SpuCategoryController spuCategoryController;

    private MockMvc mockMvc;

    @Resource
    private TestConfig testConfig;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(spuCategoryController).build());
    }

    @Test
    void postSpuCategory() throws Exception {
        SpuCategoryFormVo form = new SpuCategoryFormVo();
        form.setVisible(false);
        form.setWeight(15);
        form.setCategoryName("小米14ultra");
        form.setParentCategoryId(7);
        form.setCategoryLevel(0);
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/spu-category/manager", strJson);

    }

    @Test
    void poutSpuCategory() throws Exception {
        UpdateSpuCategoryFormVo form = new UpdateSpuCategoryFormVo();

        form.setCategoryName("小米14ultra");
        form.setCategoryId(10);

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/spu-category/manager", strJson);

    }

    @Test
    void listSpuCategoryByCondition() throws Exception {

        List<QueryParam> params = new ArrayList<>();
        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);
        String strJson = JSON.toJSONString(query);

        testConfig.getMockMvcResult("/aftersales/spu-category/manager",strJson);

    }

    @Test
    void listSpuCategory4Client() throws Exception {
        testConfig.getMockMvcResult("/aftersales/spu-category/manager");

    }

    @Test
    void deleteSpuCategoryById() throws Exception {
        Integer categoryId = 10;
        testConfig.deleteMockMvcResult("/aftersales/spu-category/manager" + categoryId);

    }


    @Test
    void setVisible() throws Exception {
        SpuCategoryVisibleSetFormVo form = new SpuCategoryVisibleSetFormVo();
        form.setVisible(true);
        form.setCategoryId(8);
        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/spu-category/manager/visible", strJson);
    }
}