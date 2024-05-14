package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel(value = "EmployeeInfo对象", description = "员工信息表")
public class EmployeeInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("登录ID")
    @TableId(value = "login_id", type = IdType.AUTO)
    private String loginId;

    @ApiModelProperty("逻辑删除")
    @TableLogic
    private Long deleted;

    @ApiModelProperty("创建时间")
    private LocalDateTime createdTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updatedTime;

    @ApiModelProperty("创建者")
    private String createdId;

    @ApiModelProperty("更新者")
    private String updatedId;

    @ApiModelProperty("员工角色")
    private Byte employeeRole;

    @ApiModelProperty("员工姓名")
    private String realName;

    @ApiModelProperty("入职时间")
    private LocalDateTime entryTime;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("员工个人简介")
    private String employeeDesc;

    @ApiModelProperty("员工照片文件ID")
    private String employeePicFileId;

    @ApiModelProperty("员工唯一工号")
    private Integer workNo;

    @ApiModelProperty("直属上级")
    private String directLeaderMobileId;
}
