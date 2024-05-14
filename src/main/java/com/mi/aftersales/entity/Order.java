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
@ApiModel(value = "Order对象", description = "工单")
public class Order implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("工单ID")
    @TableId(value = "order_id", type = IdType.AUTO)
    private String orderId;

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

    @ApiModelProperty("最小销售单元ID(商品)")
    private String skuId;

    @ApiModelProperty("发票ID")
    private String fapiaoId;

    @ApiModelProperty("商品序列号")
    private String sn;

    @ApiModelProperty("工单类型;1:到店;2寄修")
    private Byte orderType;

    @ApiModelProperty("客户故障描述")
    private String clientFaultDesc;

    @ApiModelProperty("工程师备注")
    private String engineerNotice;

    @ApiModelProperty("工程师故障描述")
    private String engineerFaultDesc;

    @ApiModelProperty("工单状态")
    private Byte orderStatus;

    @ApiModelProperty("物料费;用户支付物料费,默认所有物料总价*1.2")
    private BigDecimal materialFee;

    @ApiModelProperty("工程师手工费;工程师手工费,默认50")
    private BigDecimal manualFee;

    @ApiModelProperty("受理工程师ID")
    private String engineerLoginId;

    @ApiModelProperty("工单创建客户ID")
    private String clientLoginId;

    @ApiModelProperty("服务中心ID")
    private String centerId;

    @ApiModelProperty("到店时间/取件时间")
    private LocalDateTime arrivalTime;

    @ApiModelProperty("是否流转中")
    private Boolean transferring;

    @ApiModelProperty("流转次数")
    private Integer transferNum;
}
