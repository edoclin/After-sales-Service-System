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
@Schema(title = "客户查询地址", description = "客户查询地址")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientAddressVo {
    @Schema(description = "地址Id")
    private String addressId;

    @Schema(description = "区域")
    private String region;

    @Schema(description = "详细地址")
    private String addressDetail;

    @Schema(description = "是否默认")
    private Boolean defaulted;

    @Schema(description = "联系人")
    private String receiver;

    @Schema(description = "联系电话")
    private String mobile;
}
