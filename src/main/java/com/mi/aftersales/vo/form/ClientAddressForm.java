package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "客户添加地址", description = "客户添加地址")
public class ClientAddressForm {
    @Schema(description = "收货人")
    @NotEmpty(message = "收货人不能为空")
    private String receiver;

    @Schema(description = "是否默认")
    private Boolean defaulted = false;

    @Schema(description = "联系电话")
    @NotEmpty(message = "联系电话不能为空")
    @Pattern(regexp = "^1[345789]\\d{9}", message = "手机号格式错误")
    private String mobile;

    @Schema(description = "区域")
    @NotEmpty(message = "区域不能为空")
    private String region;

    @Schema(description = "详细地址")
    @NotEmpty(message = "详细地址不能为空")
    private String addressDetail;
}
