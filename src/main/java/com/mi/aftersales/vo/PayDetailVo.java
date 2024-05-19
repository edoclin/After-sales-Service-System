package com.mi.aftersales.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "支付详情", description = "支付详情")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PayDetailVo {
    @Schema(description = "支付时间")
    private LocalDateTime paidTime;
}
