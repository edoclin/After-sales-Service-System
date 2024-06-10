package com.mi.aftersales.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.TestConfig;
import com.mi.aftersales.exception.graceful.*;
import com.mi.aftersales.pojo.vo.form.*;
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
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

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

    String orderId = "1800044842059816960";

    @BeforeEach
    void setUp() {
        testConfig.setMockMvc(MockMvcBuilders.standaloneSetup(orderController).build());
    }

    @Test
    void postOrder() throws Exception {//1.1客户创建工单

        String[] fileIds = {"1795362152648097792"};
        ClientOrderFormVo form = new ClientOrderFormVo();
        form.setSkuId("1791492367829008384");
        form.setFapiaoId("1796427448704016384");
        form.setSn("QY1234");
        form.setOrderType("SEND_FOR1");
        form.setClientFaultDesc("test");
        form.setCenterId("String");
        form.setArrivalTime(LocalDateTime.of(2024, 6, 30, 12, 21));
        form.setFileIds(fileIds);
        form.setAddressId("1797285355565862912");
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/order/client/create", strJson);
    }

    @Test
    void listClientOrder() throws Exception {//1.2客户查询工单
        List<QueryParam> params = new ArrayList<>();

        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);

        String strJson = JSON.toJSONString(query);

        testConfig.postMockMvcResult("/aftersales/order/client", strJson);
    }

    @Test
    void orderDetailById() throws Exception {//1.3 客户查询工单详情

        testConfig.getMockMvcResult("/aftersales/order/client/detail/" + orderId);
    }

    @Test
    void listPendingOrder() throws Exception {//2.1工程师查询待办工单
        Integer spuCategoryId = 10;

        testConfig.getMockMvcResult("/aftersales/order/engineer/pending/" + spuCategoryId);

    }

    @Test
    void engineerAcceptOrder() throws Exception {//2.2 工程师接受工单

        testConfig.getMockMvcResult("/aftersales/order/engineer/accept/" + orderId);
    }

    @Test
    void listEngineerOrder() throws Exception {//2.3工程师查询所属工单
        List<QueryParam> params = new ArrayList<>();
        ConditionQuery query = new ConditionQuery();
        query.setCurrent(1L);
        query.setLimit(10L);
        query.setParams(params);
        String strJson = JSON.toJSONString(query);

        testConfig.postMockMvcResult("/aftersales/order/engineer", strJson);
    }

    @Test
    void orderDetailByEngineerId() throws Exception {//2.4工程师查询工单详情

        testConfig.getMockMvcResult("/aftersales/order/engineer/detail/" + orderId);
    }

    @Test
    void testEngineerAcceptOrder() throws Exception {//3.1 工程师上传检测前图片

        String[] fileIds = {"1800068261165916160"};
        EngineerUploadFormVo form = new EngineerUploadFormVo();
        form.setOrderId(orderId);
        form.setFileIds(fileIds);
        String strJson = JSON.toJSONString(form);

        testConfig.postMockMvcResult("/aftersales/order/engineer/upload/before", strJson);
    }

    @Test
    void startRepairMachine() throws Exception {//3.2工程师开始检测

        testConfig.putMockMvcResult("/aftersales/order/engineer/checking/" + orderId);
    }

    @Test
    void faultDescription() throws Exception {//4.1工程师上传故障描述
        FaultDescriptionFormVo form = new FaultDescriptionFormVo();
        form.setOrderId(orderId);
        form.setEngineerNotice("test");
        form.setEngineerFaultDesc("test111");

        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/order/engineer/fault-desc", strJson);
    }

    @Test
    void engineerFeeConfirm() throws Exception {//4.2 工程师确认计费

        List<MaterialNumFormVo> materials = new ArrayList<>();

        OrderFeeConfirmFormVo form = new OrderFeeConfirmFormVo();
        form.setOrderId(orderId);
        form.setMaterials(materials);
        form.setManualFee(BigDecimal.valueOf(100));
        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/order/engineer/fee-confirm", strJson);
    }

    @Test
    void clientConfirmFee() throws Exception {//5.1 用户确认计费（确认维修）

        testConfig.putMockMvcResult("/aftersales/order/client/fee-confirm/" + orderId);
    }

    @Test
    void clientRejectRepair() throws Exception {//5.2 用户拒绝维修（返回物品）

        testConfig.putMockMvcResult("/aftersales/order/client/reject/" + orderId);
    }

    @Test
    void materialApply() throws Exception {//6 工程师申请物料

        testConfig.putMockMvcResult("/aftersales/order/engineer/material-apply/" + orderId);
    }

    @Test
    void materialDistribute() throws Exception {//7 库管处理请求（分发物料）

        testConfig.putMockMvcResult("/aftersales/order/manager/material-distribute/" + orderId);
    }

    @Test
    void engineerStartRepairing() throws Exception {//8 工程师开始维修

        Boolean material = true;

        testConfig.putMockMvcResult("/aftersales/order/engineer/repair/" + orderId+ "/" + material);
    }


    @Test
    void engineerStartRechecking() throws Exception {//9 工程师开始复检

        testConfig.putMockMvcResult("/aftersales/order/engineer/recheck/" + orderId);

    }

    @Test
    void engineerUploadVideo() throws Exception {//10.1 工程师上传维修结果（视频）

        String[] fileIds = {"1800075352312111104"};

        EngineerUploadFormVo form = new EngineerUploadFormVo();
        form.setOrderId(orderId);
        form.setFileIds(fileIds);
        String strJson = JSON.toJSONString(form);

        testConfig.putMockMvcResult("/aftersales/order/engineer/recheck/upload/", strJson);
    }

    @Test
    void engineerFinishedRepair() throws Exception {//10.2 工程师完成维修，发送账单，等待支付

        testConfig.putMockMvcResult("/aftersales/order/engineer/to-be-paid/" + orderId);
    }

    @Test
    void engineerReturn() throws Exception {

        testConfig.putMockMvcResult("/aftersales/order/engineer/return/" + orderId);

    }

    @Test
    void clientClose() throws Exception {

        testConfig.putMockMvcResult("/aftersales/order/client/close/" + orderId);
    }

}