package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 登录表
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_login")
@ApiModel(value = "Login对象", description = "登录表")
public class Login implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("登录ID")
    @TableId(value = "login_id", type = IdType.ASSIGN_ID)
    private String loginId;

    @ApiModelProperty("唯一手机号")
    private String mobile;

    @ApiModelProperty("逻辑删除")
    @TableLogic
    private Long deleted;

    @ApiModelProperty("注册时间")
    private LocalDateTime createdTime;

    @ApiModelProperty("上次登录时间")
    private LocalDateTime updatedTime;

    @ApiModelProperty("创建者")
    private String createdId;

    @ApiModelProperty("更新者")
    private String updatedId;

    @ApiModelProperty("来源")
    private LoginOAuthSourceEnum source;

    @ApiModelProperty("三方登录唯一标志")
    private String appId;

    @ApiModelProperty("登录类型;1:客户, 2:员工")
    private LoginTypeEnum loginType;
}
