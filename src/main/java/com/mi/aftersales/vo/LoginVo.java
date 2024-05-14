package com.mi.aftersales.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 客户联系地址
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "登录返回对象", description = "")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class LoginVo {
    @Schema(description = "Token名称")
    private String tokenName;
    @Schema(description = "Token值")
    private String tokenValue;
    @Schema(description = "用户ID")
    private String loginId;
    @Schema(description = "其他信息")
    private String note;


}
