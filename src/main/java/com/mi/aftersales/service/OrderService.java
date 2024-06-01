package com.mi.aftersales.service;

import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.form.*;
import com.mi.aftersales.vo.result.ClientOrderDetailVo;
import com.mi.aftersales.vo.result.ClientOrderSimpleVo;
import com.mi.aftersales.vo.result.EngineerSimpleOrderVo;
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

    String NAMESPACE_4_PENDING_ORDER = "order:pending:";
    String NAMESPACE_4_MACHINE_STATE = "machine:persist:";
    String CREATED = "CREATED";
    String WAITING = "WAITING";
    String ACCEPTED = "ACCEPTED";
    String CHECKING = "CHECKING";
    String FEE_CONFIRMING = "FEE_CONFIRMING";
    String FEE_CONFIRMED = "FEE_CONFIRMED";
    String MATERIAL_APPLYING = "MATERIAL_APPLYING";
    String MATERIAL_DISTRIBUTING = "MATERIAL_DISTRIBUTING";
    String REPAIRING = "REPAIRING";
    String RE_CHECKING = "RE_CHECKING";
    String TO_BE_PAID = "TO_BE_PAID";
    String PAID = "PAID";
    String RETURNING = "RETURNING";
    String CLOSED = "CLOSED";
    String STATE_MACHINE_HEADER_ORDER_NAME = "order-id";
    String STATE_MACHINE_HEADER_CATEGORY_ID = "category-id";
    String CLIENT_CHOICE = "client-choice";
    String ENGINEER_CHOICE = "engineer-choice";

//    String STATE_MACHINE_HEADER_ORDER_NAME = "orderName";
//    String CLIENT_CHOICE = "clientChoice";
//    String ENGINEER_CHOICE = "engineerChoice";
//    String NAMESPACE_4_PENDING_ORDER = "namespace:pending:order";

    /**
     * 查询客户工单列表。
     *
     * @param query 查询条件
     * @param loginId 登录ID
     * @return 工单列表
     */
    List<ClientOrderSimpleVo> listClientOrders(ConditionQuery query, String loginId);

    /**
     * 查询客户工单详情。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     * @return 工单详情
     */
    ClientOrderDetailVo getClientOrderDetail(String orderId, String loginId);

    /**
     * 创建工单。
     *
     * @param form 工单表单
     * @param loginId 登录ID
     */
    void createOrder(ClientOrderForm form, String loginId);

    /**
     * 查询待办工单。
     *
     * @return 待办工单列表
     */
    List<EngineerSimpleOrderVo> listPendingOrders(Integer spuCategoryId);

    /**
     * 接受工单。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void acceptOrder(String orderId, String loginId);

    void engineerUploadImage(EngineerUploadForm form);

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
     * @param form 故障描述表单
     * @param loginId 登录ID
     */
    void uploadFaultDescription(FaultDescriptionForm form, String loginId);

    /**
     * 确认计费。
     *
     * @param form 计费确认表单
     * @param loginId 登录ID
     */
    void confirmFee(OrderFeeConfirmForm form, String loginId);

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
     * 分发物料。
     *
     * @param form 分发物料表单
     * @param loginId 登录ID
     */
    void distributeMaterial(MaterialDistributeForm form, String loginId);

    /**
     * 开始维修。
     *
     * @param orderId 工单ID
     * @param material 是否需要物料
     * @param loginId 登录ID
     */
    void startRepair(String orderId, Boolean material, String loginId);

    /**
     * 开始复检。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void startRechecking(String orderId, String loginId);

    void engineerUploadVideo(EngineerUploadForm form);

    /**
     * 完成维修，发送账单，等待支付。
     *
     * @param orderId 工单ID
     * @param loginId 登录ID
     */
    void finishRepair(String orderId, String loginId);

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
