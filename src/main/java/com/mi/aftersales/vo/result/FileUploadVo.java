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
@Schema(title = "上传文件响应", description = "上传文件响应")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class FileUploadVo {
    @Schema(description = "文件ID")
    private String fileId;
    @Schema(description = "存储桶的accessKey")
    private String accessKey;
}
