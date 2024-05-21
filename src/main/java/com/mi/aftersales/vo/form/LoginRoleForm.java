package com.mi.aftersales.vo.form;

import com.mi.aftersales.entity.LoginRole;
import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "添加用户角色表单", description = "添加用户角色表单")
public class LoginRoleForm {

    @Schema(description = "登录账户Id")
    @NotEmpty(message = "登录账户Id不能为空")
    private String loginId;

    @Schema(description = "角色列表")
    private EmployeeRoleEnum[] roles;
}
