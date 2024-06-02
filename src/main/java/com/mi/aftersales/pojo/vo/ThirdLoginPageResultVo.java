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
@Schema(title = "渲染三方页面", description = "三方登录页面")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ThirdLoginPageResultVo {
    @Schema(description = "三方登录页面url", defaultValue = "")
    private String url;

}
