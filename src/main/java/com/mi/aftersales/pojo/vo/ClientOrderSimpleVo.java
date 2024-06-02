package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "客户工单信息（简单）", description = "客户工单信息（简单）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientOrderSimpleVo {
    @Schema(description = "工单Id")
    private String orderId;
    @Schema(description = "产品序列号")
    private String sn;
    @Schema(description = "下单时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "工单状态")
    private String orderStatus;
    private Integer orderStatusValue;
    @Schema(description = "故障描述")
    private String clientFaultDesc;
}
