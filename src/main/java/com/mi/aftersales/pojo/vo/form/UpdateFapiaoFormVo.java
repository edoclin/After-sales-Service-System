package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "更新客户发票", description = "更新客户发票")
public class UpdateFapiaoFormVo {
    @Schema(description = "发票编号")
    @NotEmpty(message = "发票编号不能为空")
    private String fapiaoId;

    @Schema(description = "发票信息")
    private String fapiaoInfo;

    @Schema(description = "开票时间")
    @NotNull(message = "开票时间不能为空")
    private LocalDateTime fapiaoTime;
}
