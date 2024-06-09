package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "关联用户权限", description = "关联用户权限")
public class LoginPermissionFormVo {
    @Schema(description = "用户Id")
    @NotEmpty(message = "用户Id不能为空")
    private String loginId;

    @Schema(description = "权限Id")
    @NotEmpty(message = "权限Id不能为空")
    private String permissionId;
}
