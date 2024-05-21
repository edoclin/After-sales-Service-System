package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.entity.enums.EmployeeRoleEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 员工信息表
 * </p>
 *
 * @author edoclin
 * @since 2024-05-18
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_login_role")
@Schema(name = "LoginRole", description = "员工信息表")
public class LoginRole implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "登录ID")
    @TableId(value = "role_id", type = IdType.ASSIGN_UUID)
    private String roleId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "登录Id")
    private String loginId;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId;

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId;

    @Schema(description = "员工角色")
    private EmployeeRoleEnum employeeRole;
}
