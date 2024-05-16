package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "更新客户服务中心", description = "更新客户服务中心")
public class UpdateClientServiceCenterForm {
    @Schema(description = "服务中心编号")
    @NotEmpty(message = "服务中心编号不能为空")
    private String centerId;

    @Schema(description = "所属区域")
    @NotEmpty(message = "所属区域不能为空")
    private String region;

    @Schema(description = "详细地址")
    @NotEmpty(message = "详细地址不能为空")
    private String addressDetail;

    @Schema(description = "维修中心描述")
    private String centerDesc;

    @Schema(description = "联系电话")
    @NotEmpty(message = "联系电话不能为空")
    private String mobile;

    @Schema(description = "客户服务中心名称")
    private String centerName;
}
