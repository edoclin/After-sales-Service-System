package com.mi.aftersales.vo.result;

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
@Schema(title = "支付记录", description = "支付记录")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PayOrderResult {
    @Schema(description = "支付Id")
    private String payOrderId;

    @Schema(description = "工单Id")
    private String orderId;

    @Schema(description = "支付方式")
    private String payMethod;

    @Schema(description = "创建时间")
    private String createdTime;

    @Schema(description = "支付时间")
    private String updatedTime;

    @Schema(description = "支付金额")
    private String amount;

    @Schema(description = "支付状态")
    private String payStatus;
}
