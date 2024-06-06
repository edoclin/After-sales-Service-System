package com.mi.aftersales.controller;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.pojo.vo.form.FileFormVo;
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
    private TestConfig testConfig;

    @Resource
    private FileController fileController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc( MockMvcBuilders.standaloneSetup(fileController).build());
    }

    @Test
    void postFile() throws Exception {
        String[] keys = {"/avatar/test1.jpg",
                "/avatar/test2.jpg",
                "/avatar/test3.jpg"};
        FileFormVo form = new FileFormVo();
        form.setKeys(CollUtil.toList(keys));
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/file/upload", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }
}