package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.util.view.anno.View;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

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
    @View(label = "创建时间", fixed = true, sortable = true)
    private String createdTime;

    @Schema(description = "更新时间")
    @View(label = "更新时间", fixed = true, sortable = true)
    private String updatedTime;

    @Schema(description = "日志Id")
    @View(label = "日志Id", fixed = true, sortable = true, index = 1)
    private String logId;

    @Schema(description = "推送类型")
    @View(label = "推送类型", fixed = true, sortable = true, index = 2)
    private String smsType;

    @Schema(description = "目标手机号")
    @View(label = "目标手机号", fixed = true, sortable = true, index = 3)
    private String mobile;

    @Schema(description = "推送结果")
    private String result;

    @Schema(description = "响应结果")
    private String response;

    @Schema(description = "推送详情")
    private String detail;
}
