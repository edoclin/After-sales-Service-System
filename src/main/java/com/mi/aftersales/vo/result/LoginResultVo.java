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
@Schema(title = "登录返回对象", description = "")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class LoginResultVo {
    @Schema(description = "Token名称")
    private String tokenName;
    @Schema(description = "Token值")
    private String tokenValue;
    @Schema(description = "用户ID")
    private String loginId;
    @Schema(description = "其他信息")
    private String note;
    @Schema(description = "是否需要绑定手机号")
    private Boolean needMobile;
    @Schema(description = "绑定手机临时Token")
    private String tempToken;
}
