package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工单维修预估费用", description = "工单维修预估费用")
public class OrderFeeConfirmFormVo {
    @Schema(description = "工单Id")
    @NotEmpty(message = "工单Id")
    private String orderId;

    @Schema(description = "工单预估物料数量")
    private List<MaterialNumFormVo> materials;

    @Schema(description = "工程师预估手工费")
    private BigDecimal manualFee = BigDecimal.valueOf(50L);
}
