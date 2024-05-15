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
@Schema(title = "验证码获取返回对象", description = "")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SmsResultVo {
    @Schema(description = "详情")
    private String info;
    @Schema(description = "是否发送成功")
    private Boolean success;
}
