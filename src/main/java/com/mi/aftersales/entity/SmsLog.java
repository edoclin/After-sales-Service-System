package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.enums.controller.SmsType;
import com.mi.aftersales.enums.entity.SmsResultEnum;
import com.mi.aftersales.enums.entity.SmsTypeEnum;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 短信推送日志
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_sms_log")
@Schema(name = "SmsLog", description = "短信推送日志")
public class SmsLog implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    @EnableQuery
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    @EnableQuery
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId = "";

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "日志ID")
    @TableId(value = "log_id", type = IdType.ASSIGN_UUID)
    private String logId;

    @Schema(description = "推送类型")
    @EnableQuery
    private SmsTypeEnum smsType;

    @Schema(description = "目标手机号")
    @EnableQuery
    private String mobile;

    @Schema(description = "推送结果")
    @EnableQuery
    private SmsResultEnum result;

    @Schema(description = "响应结果")
    private String response;

    @Schema(description = "推送详情")
    private String detail;
}
