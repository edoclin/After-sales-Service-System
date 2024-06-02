package com.mi.aftersales.pojo.vo.form;

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
@Schema(title = "工程师上传维修文件", description = "工程师上传维修文件")
public class EngineerUploadFormVo {
    @Schema(description = "关联工单Id")
    @NotEmpty(message = "关联工单Id不能为空")
    private String  orderId;
    @Schema(description = "文件Id")
    private String[]  fileIds;
}
