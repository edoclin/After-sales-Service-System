package com.mi.aftersales.controller;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.mi.aftersales.vo.form.FileForm;
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

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author QYenon
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class FileControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(FileControllerTest.class);

    @Resource
    private FileController fileController;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(fileController).build();
    }

    @Test
    void postFile() throws Exception {
        String[] keys = {"/avatar/test1.jpg",
                "/avatar/test2.jpg",
                "/avatar/test3.jpg"};
        FileForm form = new FileForm();
        form.setKeys(CollUtil.toList(keys));
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = mockMvc.perform(
                        MockMvcRequestBuilders.post("/aftersales/file/upload")
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