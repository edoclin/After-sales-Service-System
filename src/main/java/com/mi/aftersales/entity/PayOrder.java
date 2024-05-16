package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 工单支付记录
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_pay_order")
@Schema(name = "PayOrder", description = "工单支付记录")
public class PayOrder implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
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

    @Schema(description = "工单支付ID")
    @TableId(value = "pay_order_id", type = IdType.ASSIGN_UUID)
    private String payOrderId;

    @Schema(description = "工单ID")
    private String orderId;

    @Schema(description = "支付方式")
    private Byte payMethod;

    @Schema(description = "支付记录详情(JSON)")
    private String payDetail;

    @Schema(description = "订单金额")
    private BigDecimal amount;

    @Schema(description = "支付状态")
    private Byte payStatus;
}
