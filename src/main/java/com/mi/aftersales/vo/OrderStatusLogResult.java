package com.mi.aftersales.vo;

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
@Schema(title = "工单状态变更日志", description = "工单状态变更日志")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class OrderStatusLogResult {
    @Schema(description = "工单Id")
    private String orderId;
    @Schema(description = "工单状态")
    private String orderStatus;
    @Schema(description = "状态详情")
    private String statusDetail;
    @Schema(description = "更新时间")
    private String updatedTime;
}
