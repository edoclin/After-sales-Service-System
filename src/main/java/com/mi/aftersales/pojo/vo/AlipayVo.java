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
@Schema(title = "支付宝支付表单", description = "支付宝支付表单")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class AlipayVo {
    @Schema(description = "支付表单")
    private String body;
}
