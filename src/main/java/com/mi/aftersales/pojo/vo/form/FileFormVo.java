package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.util.ArrayList;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "文件上传表单", description = "文件上传表单")
public class FileFormVo {
    @Schema(description = "COS返回的AccessKey")
    @NotEmpty(message = "COS返回的AccessKey不能为空")
    private List<String> keys = new ArrayList<>();
}
