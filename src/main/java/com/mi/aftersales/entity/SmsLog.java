package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
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
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    private String createdId;

    @Schema(description = "更新者")
    private String updatedId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "日志ID")
    private String logId;

    @Schema(description = "推送类型")
    private Byte smsType;

    @Schema(description = "目标手机号")
    private String mobile;

    @Schema(description = "推送结果")
    private Byte result;

    @Schema(description = "响应结果")
    private String response;

    @Schema(description = "推送详情")
    private String detail;
}
