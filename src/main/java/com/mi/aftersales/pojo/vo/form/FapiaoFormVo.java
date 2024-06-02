package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "客户创建发票", description = "客户创建发票")
public class FapiaoFormVo {
    @Schema(description = "发票代码")
    @NotEmpty(message = "发票代码不能为空")
    private String fapiaoCode;

    @Schema(description = "发票号码")
    @NotEmpty(message = "发票号码不能为空")
    private String fapiaoNo;

    @Schema(description = "发票信息")
    private String fapiaoInfo;

    @Schema(description = "开票时间")
    private LocalDateTime fapiaoTime;
}
