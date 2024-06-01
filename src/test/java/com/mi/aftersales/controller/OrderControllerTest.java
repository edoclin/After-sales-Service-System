package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
import com.mi.aftersales.vo.MaterialNum;
import com.mi.aftersales.vo.form.*;
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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @author QYenon
 * @create 2024/5/30
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@WebAppConfiguration
class OrderControllerTest {

    private static final Logger logger = LoggerFactory.getLogger(OrderControllerTest.class);

    @Resource
    private TestConfig testConfig;

    @Resource
    private OrderController orderController;

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(orderController).build());
    }

    @Test
    void listClientOrder() throws Exception {
        List<QueryParam> params = new ArrayList<>();

        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);

        String strJson = JSON.toJSONString(query);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/client", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void orderDetailById() throws Exception {
        String orderId = "1796166211369848832";


        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/order/client/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void postOrder() throws Exception {

        String[] fileIds = {"1795362152648097792"};
        ClientOrderForm form = new ClientOrderForm();
        form.setSkuId("1791492367829008384");
        form.setFapiaoId("1794933315766226944");
        form.setSn("QY1234");
        form.setOrderType("SEND_FOR");
        form.setClientFaultDesc("test");
        form.setCenterId("String");
        form.setArrivalTime(LocalDateTime.of(2024,6,5,12,21));
        form.setFileIds(fileIds);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/order/client/create", strJson);


        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void listPendingOrder() throws Exception {
        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/order/engineer/pending");

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerAcceptOrder() throws Exception {
        String orderId = "1796192674496368640";


        MvcResult mvcResult = testConfig.getMockMvcResult("/aftersales/order/engineer/accept/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void testEngineerAcceptOrder() throws Exception {

        String[] fileIds = {"1796427449530294272"};
        EngineerUploadForm form = new EngineerUploadForm();
        form.setOrderId("1796192674496368640");
        form.setFileIds(fileIds);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.postMockMvcResult("/aftersales/order/engineer/upload/image", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void startRepairMachine() throws Exception {
        String orderId = "1796192674496368640";

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/checking/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void faultDescription() throws Exception {
        FaultDescriptionForm form = new FaultDescriptionForm();
        form.setOrderId("1796192674496368640");
        form.setEngineerNotice("test");
        form.setEngineerFaultDesc("test111");

        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/faultDesc", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerFeeConfirm() throws Exception {

        List<MaterialNum> materials = new ArrayList<>();

        OrderFeeConfirmForm form = new OrderFeeConfirmForm();
        form.setOrderId("1796192674496368640");
        form.setMaterials(materials);
        form.setManualFee(BigDecimal.valueOf(100));
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/feeConfirm", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void clientConfirmFee() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/client/feeConfirm/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void clientRejectRepair() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/client/reject/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void materialApply() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/material/apply/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void materialDistribute() throws Exception {
        List<MaterialNum> materials = new ArrayList<>();

        MaterialDistributeForm form = new MaterialDistributeForm();
        form.setOrderId("1796192674496368640");
        form.setMaterials(materials);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/material/distribute/", strJson);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerStartRepairing() throws Exception {
        String orderId = "1796192674496368640";
        Boolean material = true;

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/repair/" + orderId+ "/" + material);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerStartRechecking() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/recheck/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerUploadVideo() throws Exception {

        String[] fileIds = {"1795362152648097792"};

        EngineerUploadForm form = new EngineerUploadForm();
        form.setOrderId("1796192674496368640");
        form.setFileIds(fileIds);
        String strJson = JSON.toJSONString(form);

        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/recheck/upload/", strJson);
        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerFinishedRepair() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/toBePaid/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void engineerReturn() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/engineer/return/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }

    @Test
    void clientClose() throws Exception {
        String orderId = "1796192674496368640";
        MvcResult mvcResult = testConfig.putMockMvcResult("/aftersales/order/client/close/" + orderId);

        logger.info("调用返回的结果：{}", mvcResult.getResponse().getContentAsString());
    }


}