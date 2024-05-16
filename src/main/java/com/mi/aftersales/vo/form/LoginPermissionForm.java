package com.mi.aftersales.vo.form;

import com.mi.aftersales.entity.enums.OrderTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "关联用户权限", description = "关联用户权限")
public class LoginPermissionForm {
    @Schema(description = "用户ID")
    @NotEmpty(message = "用户ID不能为空")
    private String loginId;

    @Schema(description = "权限ID")
    @NotEmpty(message = "权限ID不能为空")
    private String permissionId;
}
