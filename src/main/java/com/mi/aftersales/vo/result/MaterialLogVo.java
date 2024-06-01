package com.mi.aftersales.vo.result;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "物料变更日志", description = "物料变更日志")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class MaterialLogVo {
    @Schema(description = "物料日志Id")
    private String materialLogId;
    @Schema(description = "物料Id")
    private String materialId;
    @Schema(description = "操作者Id")
    private String operatorId;
    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "操作")
    private String action;
    @Schema(description = "变更量")
    private String delta;
    @Schema(description = "变更描述")
    private String logDetail;
}
