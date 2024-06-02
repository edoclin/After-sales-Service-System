package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
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
class SpuCategoryControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(SpuCategoryControllerTest.class);

    @Resource
    private SpuCategoryController spuCategoryController;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(spuCategoryController).build();
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

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.post("/aftersales/spuCategory/")
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
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
    void listSpuCategory4Client() throws Exception {
        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.get("/aftersales/spuCategory/list/client")
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());

    }

    @Test
    void setVisible() throws Exception {
        SpuCategoryVisibleSetFormVo form = new SpuCategoryVisibleSetFormVo();
        form.setVisible(true);
        form.setCategoryId(8);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.get("/aftersales/spuCategory/visible")
                                .header("aftersales-token", "Bearer 4V_3nTMacaHes5kbk_T6rKdccUrQb0c5zU__")
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