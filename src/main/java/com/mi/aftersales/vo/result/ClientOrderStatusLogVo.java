package com.mi.aftersales.vo.result;

import com.fasterxml.jackson.annotation.JsonInclude;
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
@Schema(title = "工单状态转换日志", description = "工单状态转换日志")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientOrderStatusLogVo {
    @Schema(description = "地址Id")
    private String logId;

    @Schema(description = "创建时间")
    private String createdTime;

    @Schema(description = "更新时间")
    private String updatedTime;

    @Schema(description = "工单状态")
    private String orderStatus;

    @Schema(description = "工单状态值")
    private Integer orderStatusValue;

    @Schema(description = "详情")
    private String statusDetail;
}
