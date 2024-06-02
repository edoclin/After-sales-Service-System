package com.mi.aftersales.pojo.vo;

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
@Schema(title = "客户端展示文件", description = "客户端展示文件")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class FileVo {
    @Schema(description = "文件ID")
    private String fileId;
    @Schema(description = "访问路径")
    private String url;
    @Schema(description = "文件类型")
    private String type;
}
