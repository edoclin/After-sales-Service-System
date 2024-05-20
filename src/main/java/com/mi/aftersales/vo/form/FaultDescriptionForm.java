package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工程师故障描述", description = "工程师故障描述")
public class FaultDescriptionForm {

    @Schema(description = "工单ID")
    @NotEmpty(message = "工单ID不能为空")
    private String orderId;

    @Schema(description = "工程师备注")
    private String engineerNotice;

    @Schema(description = "工程师故障描述")
    private String engineerFaultDesc;

}
