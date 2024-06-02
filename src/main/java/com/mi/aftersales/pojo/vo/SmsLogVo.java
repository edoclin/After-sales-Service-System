package com.mi.aftersales.pojo.vo;

import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.enums.entity.SmsResultEnum;
import com.mi.aftersales.enums.entity.SmsTypeEnum;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "短信提醒日志", description = "短信提醒日志")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SmsLogVo {
    @Schema(description = "创建时间")
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    private LocalDateTime updatedTime;

    @Schema(description = "日志ID")
    private String logId;

    @Schema(description = "推送类型")
    @EnableQuery
    private String smsType;

    @Schema(description = "目标手机号")
    @EnableQuery
    private String mobile;

    @Schema(description = "推送结果")
    @EnableQuery
    private String result;

    @Schema(description = "响应结果")
    private String response;

    @Schema(description = "推送详情")
    private String detail;
}
