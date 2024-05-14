package com.mi.aftersales.vo.form;

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
@Schema(title = "登录表单", description = "用户登录表单")
public class LoginBySmsForm {

    @Schema(description = "登录手机号")
    @NotEmpty(message = "手机号不能为空")
    @Length(min = 11, max = 11, message = "请输入11位中国大陆手机号")
    @Pattern(regexp = "^1[345789]\\d{9}", message = "手机号格式错误")
    private String mobile;

    @Schema(description = "验证码")
    @NotEmpty(message = "验证码不能为空")
    @Length(min = 6, max = 6, message = "请输入6位有效验证码")
    private String code;

}
