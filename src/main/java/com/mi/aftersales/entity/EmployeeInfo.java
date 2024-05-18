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
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_employee_info")
@Schema(name = "EmployeeInfo", description = "员工信息表")
public class EmployeeInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "登录ID")
    @TableId(value = "login_id", type = IdType.INPUT)
    private String loginId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId = "";

    @Schema(description = "员工姓名")
    private String realName;

    @Schema(description = "入职时间")
    private LocalDateTime entryTime;

    @Schema(description = "邮箱")
    private String email;

    @Schema(description = "员工个人简介")
    private String employeeDesc;

    @Schema(description = "员工照片文件ID")
    private String employeePicFileId;

    @Schema(description = "员工唯一工号")
    private Integer workNo;

    @Schema(description = "直属上级")
    private String directLeaderMobileId;
}
