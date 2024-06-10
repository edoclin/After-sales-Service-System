package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.entity.Material;


import com.mi.aftersales.pojo.vo.form.ManagerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;


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
    private MaterialController materialController;

    @Resource
    private TestConfig testConfig;

    private MockMvc mockMvc;


    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(materialController).build());
    }

    @Test
    public void addMaterial() throws Exception {
        MaterialFormVo form = new MaterialFormVo();
        form.setMaterialName("Test2");
        form.setMaterialDesc("TestAddMaterial");
        form.setUnit("test");
        form.setCost(BigDecimal.valueOf(5));
        form.setPrice(BigDecimal.valueOf(10));
        form.setStock(BigDecimal.valueOf(20));
        form.setAlertNum(BigDecimal.valueOf(2));

        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/material/manager",strJson);

    }

    @Test
    public void updateMaterial() throws Exception {

        // 创建一个更新物料表单对象
        ManagerUpdateMaterialFormVo form = new ManagerUpdateMaterialFormVo();
        form.setMaterialId("1795750940717076480");
        form.setMaterialName("UpdatedTest1");
        form.setMaterialDesc("UpdatedTestUpdateMaterial");
        form.setUnit("test");
        form.setCost(BigDecimal.valueOf(5));
        form.setPrice(BigDecimal.valueOf(10));
        form.setStock(BigDecimal.valueOf(20));
        form.setAlertNum(BigDecimal.valueOf(2));

        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/material/manager",strJson);

    }

    @Test
    public void getMaterialDetailById() throws Exception {

        String materialId = "1795750940717076480";

        testConfig.getMockMvcResult("/aftersales/material/manager/" + materialId);
    }

    @Test
    public void conditionQuery() throws Exception {

        List<QueryParam> params = new ArrayList<>();

        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);

        String strJson = JSON.toJSONString(query);


        testConfig.postMockMvcResult("/aftersales/material/manager/query" ,strJson);
    }

    @Test
    public void deleteMaterialById() throws Exception {

        String materialId = "1795750940717076480";

        testConfig.deleteMockMvcResult("/aftersales/material/manager/" + materialId);
    }

    @Test
    public void listApplyingMaterial() throws Exception {

        List<QueryParam> params = new ArrayList<>();

        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);

        String strJson = JSON.toJSONString(query);


        testConfig.postMockMvcResult("/aftersales/material/manager/applying" ,strJson);
    }

}