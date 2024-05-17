package com.mi.aftersales.vo.form;

import com.mi.aftersales.entity.enums.OrderTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "客户创建工单", description = "客户创建工单")
public class ClientOrderForm {
    @Schema(description = "工单关联商品")
    @NotEmpty(message = "商品不能为空")
    private String skuId;

    @Schema(description = "发票ID")
    @NotEmpty(message = "发票ID不能为空")
    private String fapiaoId;

    @Schema(description = "商品序列号")
    private String sn;

    @Schema(description = "工单类型（SEND_FOR：寄修；TO_STOP：送修）")
    private OrderTypeEnum orderType = OrderTypeEnum.SEND_FOR;

    @Schema(description = "客户描述故障")
    private String clientFaultDesc;

    @Schema(description = "到店维修中心")
    private String centerId;

    @Schema(description = "取件时间/到店时间")
    private LocalDateTime arrivalTime;
}
