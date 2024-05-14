package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel(value = "PayOrder对象", description = "工单支付记录")
public class PayOrder implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("创建时间")
    private LocalDateTime createdTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updatedTime;

    @ApiModelProperty("创建者")
    private String createdId;

    @ApiModelProperty("更新者")
    private String updatedId;

    @ApiModelProperty("逻辑删除")
    @TableLogic
    private Long deleted;

    @ApiModelProperty("工单支付ID")
    @TableId(value = "pay_order_id", type = IdType.AUTO)
    private String payOrderId;

    @ApiModelProperty("工单ID")
    private String orderId;

    @ApiModelProperty("支付方式")
    private Byte payMethod;

    @ApiModelProperty("支付记录详情(JSON)")
    private String payDetail;

    @ApiModelProperty("订单金额")
    private BigDecimal amount;

    @ApiModelProperty("支付状态")
    private Byte payStatus;
}
