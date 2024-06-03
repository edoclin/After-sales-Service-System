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
@Schema(title = "客户发票信息", description = "客户发票信息")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientFapiaoVo {
    @Schema(description = "发票Id")
    private String fapiaoId;
    @Schema(description = "发票代码")
    private String fapiaoNo;
    @Schema(description = "发票号码")
    private String fapiaoCode;
    @Schema(description = "开票时间")
    private String fapiaoTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "发票详情")
    private String fapiaoInfo;
}
