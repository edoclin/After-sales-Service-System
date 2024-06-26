package com.mi.aftersales.service;

import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.ClientOrderSimpleVo;
import com.mi.aftersales.pojo.vo.OrderDetailVo;
import com.mi.aftersales.pojo.vo.OrderSimple4EngineerVo;
import com.mi.aftersales.pojo.vo.PendingOrderSimple4EngineerVo;
import com.mi.aftersales.pojo.vo.form.*;
import com.mi.aftersales.util.query.ConditionQuery;
import org.springframework.messaging.Message;

import java.util.List;

/**
 * <p>
 * 工单 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface OrderService {
    void sendSms(String orderId);

    /**
     * 查询客户工单列表。
     *
     * @param query   查询条件
     * @param loginId 登录ID
     * @return 工单列表
     */
    List<ClientOrderSimpleVo> listClientOrders(ConditionQuery query, String loginId);

    /**
     * 查询工单详情。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     * @return 工单详情
     */
    OrderDetailVo getClientOrderDetail(String orderId, String loginId, Boolean isClient);

    /**
     * 创建工单。
     *
     * @param form    工单表单
     * @param loginId 登录ID
     */
    void createOrder(ClientOrderFormVo form, String loginId);

    /**
     * 查询待办工单。
     *
     * @return 待办工单列表
     */
    List<PendingOrderSimple4EngineerVo> listPendingOrders(Integer spuCategoryId);

    /**
     * 接受工单。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void acceptOrder(String orderId, String loginId);

    PageResult<OrderSimple4EngineerVo> listEngineerOrder(ConditionQuery query);

    void engineerUploadImage(EngineerUploadFormVo form);

    /**
     * 开始检测。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void startChecking(String orderId, String loginId);

    /**
     * 上传故障描述。
     *
     * @param form    故障描述表单
     * @param loginId 登录ID
     */
    void uploadFaultDescription(FaultDescriptionFormVo form, String loginId);

    /**
     * 确认计费。
     *
     * @param form    计费确认表单
     * @param loginId 登录ID
     */
    void confirmFee(OrderFeeConfirmFormVo form, String loginId);

    /**
     * 用户确认计费（确认维修）。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void clientConfirmFee(String orderId, String loginId);

    /**
     * 用户拒绝维修（返回物品）。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void clientRejectRepair(String orderId, String loginId);

    /**
     * 工程师申请物料。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void applyMaterial(String orderId, String loginId);

    /**
     * 分发物料
     * @param loginId 登录ID
     */
    void distributeMaterial(String orderId, String loginId);

    /**
     * 开始维修。
     *
     * @param orderId  工单ID
     * @param material 是否需要物料
     * @param loginId  登录ID
     */
    void startRepair(String orderId, Boolean material, String loginId);

    /**
     * 开始复检。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void startRechecking(String orderId, String loginId);

    void engineerUploadVideo(EngineerUploadFormVo form);

    /**
     * 完成维修，发送账单，等待支付。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void finishRecheck(String orderId, String loginId);

    /**
     * 开始返还物品。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void returnItem(String orderId, String loginId);

    /**
     * 关闭工单。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void closeOrder(String orderId, String loginId);

    /**
     * 构建状态转换消息。
     *
     * @param payload 事件枚举
     * @param orderId 订单ID
     * @return 构建的消息对象
     */
    Message<OrderStatusChangeEventEnum> statusFlow(OrderStatusChangeEventEnum payload, String orderId);

    /**
     * 发送状态转换事件。
     *
     * @param message 状态转换消息
     * @return 事件发送结果
     */
    boolean sendEvent(Message<OrderStatusChangeEventEnum> message);
}
