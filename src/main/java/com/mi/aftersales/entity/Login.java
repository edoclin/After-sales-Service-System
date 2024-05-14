package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.entity.enums.LoginOAuthSourceEnum;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(name = "Login", description = "登录表")
public class Login implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "登录ID")
    @TableId(value = "login_id", type = IdType.AUTO)
    private String loginId;

    @Schema(description = "唯一手机号")
    private String mobile;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "注册时间")
    private LocalDateTime createdTime;

    @Schema(description = "上次登录时间")
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    private String createdId;

    @Schema(description = "更新者")
    private String updatedId;

    @Schema(description = "来源")
    private LoginOAuthSourceEnum source;

    @Schema(description = "三方登录唯一标志")
    private String appId;

    @Schema(description = "登录类型;1:客户, 2:员工")
    private LoginTypeEnum loginType;
}
