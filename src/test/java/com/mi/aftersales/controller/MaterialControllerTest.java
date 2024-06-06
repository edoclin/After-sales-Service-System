package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.entity.Material;


import com.mi.aftersales.pojo.vo.form.ManagerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
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



    private MockMvc mockMvc;


    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(materialController).build();
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

//        materialController.addMaterial(form);

        MvcResult mvcResult = mockMvc.perform(
                MockMvcRequestBuilders.post("/aftersales/material/")
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

        // 模拟Service层根据ID获取物料信息
        Material material = new Material();
        material.setMaterialId("1795750940717076480");
        material.setMaterialName("Test");



        // 发送PUT请求，更新物料信息
        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.put("/aftersales/material/")
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