package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.entity.enums.OrderTypeEnum;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 工单
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_order")
@Schema(name = "Order", description = "工单")
public class Order implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "工单ID")
    @TableId(value = "order_id", type = IdType.ASSIGN_UUID)
    @EnableQuery
    private String orderId;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    @EnableQuery
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    @EnableQuery
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId = "";

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "最小销售单元ID(商品)")
    private String skuId;

    @Schema(description = "发票ID")
    private String fapiaoId;

    @Schema(description = "商品序列号")
    @EnableQuery
    private String sn;

    @Schema(description = "工单类型;1:到店;2寄修")
    private OrderTypeEnum orderType;

    @Schema(description = "客户故障描述")
    private String clientFaultDesc;

    @Schema(description = "工程师备注")
    private String engineerNotice;

    @Schema(description = "工程师故障描述")
    private String engineerFaultDesc;

    @Schema(description = "工单状态")
    @EnableQuery
    private OrderStatusEnum orderStatus;

    @Schema(description = "物料费;用户支付物料费,默认所有物料总价*1.2")
    private BigDecimal materialFee;

    @Schema(description = "工程师手工费;工程师手工费,默认50")
    private BigDecimal manualFee;

    @Schema(description = "受理工程师ID")
    private String engineerLoginId;

    @Schema(description = "工单创建客户ID")
    private String clientLoginId;

    @Schema(description = "服务中心ID")
    private String centerId;

    @Schema(description = "到店时间/取件时间")
    private LocalDateTime arrivalTime;

    @Schema(description = "是否流转中")
    private Boolean transferring;

    @Schema(description = "流转次数")
    private Integer transferNum;
}
